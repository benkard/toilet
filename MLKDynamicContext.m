/* -*- mode: objc; coding: utf-8 -*- */
/* Toilet Lisp, a Common Lisp subset for the Étoilé runtime.
 * Copyright (C) 2008  Matthias Andreas Benkard.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#import <Foundation/NSArray.h>
#import <Foundation/NSDictionary.h>
#import <Foundation/NSException.h>
#import <Foundation/NSNull.h>
#import <Foundation/NSSet.h>
#import <Foundation/NSString.h>
#import <Foundation/NSThread.h>

#import "MLKBackquoteReader.h"
#import "MLKBinaryStreamCharacterStream.h"
#import "MLKCharacterStream.h"
#import "MLKCommaReader.h"
#import "MLKCons.h"
#import "MLKDispatchingMacroCharacterReader.h"
#import "MLKDynamicContext.h"
#import "MLKEnvironment.h"
#import "MLKFileHandleStream.h"
#import "MLKPackage.h"
#import "MLKParenReader.h"
#import "MLKQuoteReader.h"
#import "MLKReadtable.h"
#import "MLKRoot.h"
#import "MLKStringReader.h"
#import "MLKSemicolonReader.h"
#import "MLKSharpsignColonReader.h"
#import "MLKSymbol.h"
#import "MLKInteger.h"
#import "MLKUnboundVariableError.h"
#import "runtime-compatibility.h"
#import "util.h"


#define MAKE_ENVIRONMENT(variable, parent, parent_member)               \
  [[MLKEnvironment alloc]                                               \
    initWithParent:(parent                                              \
                    ? (id) parent_member                                \
                    : nil)                                              \
    values:variable]


static MLKDynamicContext *global_context;


@implementation MLKDynamicContext
+(void) initialize
{
  NSMutableDictionary *vars = [NSMutableDictionary dictionaryWithCapacity:64];
  MLKPackage *cl = [MLKPackage findPackage:@"COMMON-LISP"];
  MLKPackage *clUser = [MLKPackage findPackage:@"COMMON-LISP-USER"];
  MLKPackage *keyword = [MLKPackage findPackage:@"KEYWORD"];
  MLKSymbol *t = [cl intern:@"T"];
  MLKReadtable *readtable = [[MLKReadtable alloc] init];
  MLKDispatchingMacroCharacterReader *sharpsign;
  unichar ch;

  id NIL = [NSNull null];

  // Build the initial readtable.
  [readtable setSyntaxType:WHITESPACE forCharacter:'\t'];
  [readtable setConstituentTrait:INVALID forCharacter:'\t'];
  [readtable setSyntaxType:WHITESPACE forCharacter:'\n'];
  [readtable setConstituentTrait:INVALID forCharacter:'\n'];
  [readtable setSyntaxType:WHITESPACE forCharacter:'\f'];  // linefeed == newline?
  [readtable setConstituentTrait:INVALID forCharacter:'\f'];
  [readtable setSyntaxType:WHITESPACE forCharacter:'\r'];
  [readtable setConstituentTrait:INVALID forCharacter:'\r'];
  [readtable setSyntaxType:WHITESPACE forCharacter:' '];
  [readtable setConstituentTrait:INVALID forCharacter:' '];
  //  [readtable setSyntaxType:WHITESPACE forCharacter:'\Page'];
  //  [readtable setConstituentTrait:INVALID forCharacter:'\Page'];

  [readtable setSyntaxType:CONSTITUENT forCharacter:'\b'];
  [readtable setConstituentTrait:INVALID forCharacter:'\b'];
  [readtable setSyntaxType:CONSTITUENT forCharacter:'\177'];  // Rubout
  [readtable setConstituentTrait:INVALID forCharacter:'\177'];

  [readtable setSyntaxType:CONSTITUENT forCharacter:':'];
  [readtable setConstituentTrait:PACKAGE_MARKER forCharacter:':'];
  [readtable unsetConstituentTrait:ALPHABETIC forCharacter:':'];

  [readtable setSyntaxType:CONSTITUENT forCharacter:'<'];
  [readtable setSyntaxType:CONSTITUENT forCharacter:'='];
  [readtable setSyntaxType:CONSTITUENT forCharacter:'>'];
  [readtable setSyntaxType:CONSTITUENT forCharacter:'?'];
  [readtable setSyntaxType:CONSTITUENT forCharacter:'!'];
  [readtable setSyntaxType:CONSTITUENT forCharacter:'@'];
  [readtable setSyntaxType:CONSTITUENT forCharacter:'['];
  [readtable setSyntaxType:CONSTITUENT forCharacter:'$'];
  [readtable setSyntaxType:CONSTITUENT forCharacter:'%'];
  [readtable setSyntaxType:CONSTITUENT forCharacter:']'];
  [readtable setSyntaxType:CONSTITUENT forCharacter:'&'];
  [readtable setSyntaxType:CONSTITUENT forCharacter:'^'];
  [readtable setSyntaxType:CONSTITUENT forCharacter:'_'];
  [readtable setSyntaxType:CONSTITUENT forCharacter:'*'];
  [readtable setSyntaxType:CONSTITUENT forCharacter:'{'];
  [readtable setSyntaxType:CONSTITUENT forCharacter:'}'];
  [readtable setSyntaxType:CONSTITUENT forCharacter:'~'];

  [readtable setSyntaxType:CONSTITUENT forCharacter:'+'];
  [readtable setConstituentTrait:PLUS_SIGN forCharacter:'+'];
  [readtable setConstituentTrait:SIGN forCharacter:'+'];

  [readtable setSyntaxType:CONSTITUENT forCharacter:'-'];
  [readtable setConstituentTrait:MINUS_SIGN forCharacter:'-'];
  [readtable setConstituentTrait:SIGN forCharacter:'-'];

  [readtable setSyntaxType:CONSTITUENT forCharacter:'.'];
  [readtable setConstituentTrait:DOT forCharacter:'.'];
  [readtable setConstituentTrait:DECIMAL_POINT forCharacter:'.'];

  [readtable setSyntaxType:CONSTITUENT forCharacter:'/'];
  [readtable setConstituentTrait:RATIO_MARKER forCharacter:'/'];

  // Maybe distinguish different types of exponent markers as the CLHS
  // does?  For now, the MLKFloat class cluster's string-parsing
  // constructor does the discrimination.
  [readtable setConstituentTrait:EXPONENT_MARKER forCharacter:'d'];
  [readtable setConstituentTrait:EXPONENT_MARKER forCharacter:'e'];
  [readtable setConstituentTrait:EXPONENT_MARKER forCharacter:'f'];
  [readtable setConstituentTrait:EXPONENT_MARKER forCharacter:'l'];
  [readtable setConstituentTrait:EXPONENT_MARKER forCharacter:'s'];
  [readtable setConstituentTrait:EXPONENT_MARKER forCharacter:'D'];
  [readtable setConstituentTrait:EXPONENT_MARKER forCharacter:'E'];
  [readtable setConstituentTrait:EXPONENT_MARKER forCharacter:'F'];
  [readtable setConstituentTrait:EXPONENT_MARKER forCharacter:'L'];
  [readtable setConstituentTrait:EXPONENT_MARKER forCharacter:'S'];

  [readtable setConstituentTrait:NUMBER_MARKER forCharacter:'d'];
  [readtable setConstituentTrait:NUMBER_MARKER forCharacter:'e'];
  [readtable setConstituentTrait:NUMBER_MARKER forCharacter:'f'];
  [readtable setConstituentTrait:NUMBER_MARKER forCharacter:'l'];
  [readtable setConstituentTrait:NUMBER_MARKER forCharacter:'s'];
  [readtable setConstituentTrait:NUMBER_MARKER forCharacter:'D'];
  [readtable setConstituentTrait:NUMBER_MARKER forCharacter:'E'];
  [readtable setConstituentTrait:NUMBER_MARKER forCharacter:'F'];
  [readtable setConstituentTrait:NUMBER_MARKER forCharacter:'L'];
  [readtable setConstituentTrait:NUMBER_MARKER forCharacter:'S'];

  [readtable setSyntaxType:MULTI_ESCAPE forCharacter:'|'];

  [readtable setSyntaxType:NONTERMINATING_MACRO forCharacter:'#'];
  sharpsign = LAUTORELEASE ([[MLKDispatchingMacroCharacterReader
                              alloc] init]);
  [readtable setMacroFunction:sharpsign forCharacter:'#'];

  [sharpsign setMacroFunction:LAUTORELEASE([[MLKSharpsignColonReader alloc]
                                            init])
             forCharacter:':'];

  [readtable setSyntaxType:SINGLE_ESCAPE forCharacter:'\\'];
  
  [readtable setSyntaxType:TERMINATING_MACRO forCharacter:'('];
  [readtable setMacroFunction:LAUTORELEASE([[MLKParenReader alloc] init])
             forCharacter:'('];
  [readtable setSyntaxType:TERMINATING_MACRO forCharacter:')'];

  [readtable setSyntaxType:TERMINATING_MACRO forCharacter:'"'];
  [readtable setMacroFunction:LAUTORELEASE([[MLKStringReader alloc] init])
             forCharacter:'"'];

  [readtable setSyntaxType:TERMINATING_MACRO forCharacter:'\''];
  [readtable setMacroFunction:LAUTORELEASE([[MLKQuoteReader alloc] init])
             forCharacter:'\''];
  
  [readtable setSyntaxType:TERMINATING_MACRO forCharacter:'`'];
  [readtable setMacroFunction:LAUTORELEASE([[MLKBackquoteReader alloc] init])
             forCharacter:'`'];
  
  [readtable setSyntaxType:TERMINATING_MACRO forCharacter:','];
  [readtable setMacroFunction:LAUTORELEASE([[MLKCommaReader alloc] init])
             forCharacter:','];

  [readtable setSyntaxType:TERMINATING_MACRO forCharacter:';'];
  [readtable setMacroFunction:LAUTORELEASE([[MLKSemicolonReader alloc] init])
             forCharacter:';'];

  for (ch = '0'; ch <= '9'; ch++)
    {
      [readtable setSyntaxType:CONSTITUENT forCharacter:ch];
      [readtable setConstituentTrait:ALPHA_DIGIT forCharacter:ch];
      [readtable unsetConstituentTrait:ALPHABETIC forCharacter:ch];
    }

  for (ch = 'A'; ch <= 'Z'; ch++)
    {
      [readtable setSyntaxType:CONSTITUENT forCharacter:ch];
      [readtable setConstituentTrait:ALPHA_DIGIT forCharacter:ch];
      [readtable unsetConstituentTrait:ALPHABETIC forCharacter:ch];
    }
  
  for (ch = 'a'; ch <= 'z'; ch++)
    {
      [readtable setSyntaxType:CONSTITUENT forCharacter:ch];
      [readtable setConstituentTrait:ALPHA_DIGIT forCharacter:ch];
      [readtable unsetConstituentTrait:ALPHABETIC forCharacter:ch];
    }


  // FIXME: Initialise stuff.
#define INIT(VARNAME, VALUE) [vars setObject:VALUE forKey:[cl intern:VARNAME]]

  MLKFileHandleStream *ferrstream, *foutstream;
  MLKCharacterStream *errstream, *outstream;
  ferrstream = [[MLKFileHandleStream alloc]
                 initWithFileHandle:[NSFileHandle fileHandleWithStandardError]];
  foutstream = [[MLKFileHandleStream alloc]
                 initWithFileHandle:[NSFileHandle fileHandleWithStandardOutput]];
  errstream = [[MLKBinaryStreamCharacterStream alloc]
                initWithBinaryStream:ferrstream];
  outstream = [[MLKBinaryStreamCharacterStream alloc]
                initWithBinaryStream:foutstream];
  LAUTORELEASE (ferrstream);
  LAUTORELEASE (foutstream);
  LAUTORELEASE (errstream);
  LAUTORELEASE (outstream);

  INIT(@"*BREAK-ON-SIGNALS*", NIL);
  INIT(@"*COMPILE-FILE-PATHNAME*", NIL);
  INIT(@"*COMPILE-FILE-TRUENAME*", NIL);
  INIT(@"*COMPILE-PRINT*", NIL);
  INIT(@"*COMPILE-VERBOSE*", t);
  //  INIT(@"*DEBUG-IO*", );
  INIT(@"*DEBUGGER-HOOK*", NIL);
  //  INIT(@"*DEFAULT-PATHNAME-DEFAULTS*", );
  INIT(@"*ERROR-OUTPUT*", errstream);
  INIT(@"*FEATURES*", [MLKCons
                        cons:[keyword intern:@"ETOILET"]
                        with:[MLKCons
                               cons:[keyword intern:@"COMMON-LISP"]
                               with:[MLKCons
                                      cons:[keyword intern:@"ANSI-CL"]
                                      with:nil]]]);
  INIT(@"*GENSYM-COUNTER*", [MLKInteger integerWithInt:0]);
  INIT(@"*LOAD-PATHNAME*", NIL);
  INIT(@"*LOAD-PRINT*", NIL);
  INIT(@"*LOAD-TRUENAME*", NIL);
  INIT(@"*LOAD-VERBOSE*", t);
  //  INIT(@"*MACROEXPAND-HOOK*", );
  INIT(@"*MODULES*", NIL);
  INIT(@"*PACKAGE*", clUser);
  INIT(@"*PRINT-ARRAY*", t);
  INIT(@"*PRINT-BASE*", [MLKInteger integerWithInt:10]);
  INIT(@"*PRINT-CASE*", [keyword intern:@"UPCASE"]);
  INIT(@"*PRINT-CIRCLE*", NIL);
  INIT(@"*PRINT-ESCAPE*", t);
  INIT(@"*PRINT-GENSYM*", t);
  INIT(@"*PRINT-LENGTH*", NIL);
  INIT(@"*PRINT-LEVEL*", NIL);
  INIT(@"*PRINT-LINES*", NIL);
  INIT(@"*PRINT-MISER-WIDTH*", NIL);
  //  INIT(@"*PRINT-PPRINT-DISPATCH*", );
  INIT(@"*PRINT-PRETTY*", t);
  INIT(@"*PRINT-RADIX*", NIL);
  INIT(@"*PRINT-READABLY*", NIL);
  INIT(@"*PRINT-RIGHT-MARGIN*", NIL);
  //  INIT(@"*QUERY-IO*", );
  //  INIT(@"*RANDOM-STATE*", );
  INIT(@"*READ-BASE*", [MLKInteger integerWithInt:10]);
  INIT(@"*READ-DEFAULT-FLOAT-FORMAT*", [cl intern:@"SINGLE-FLOAT"]);
  INIT(@"*READ-EVAL*", t);
  INIT(@"*READ-SUPPRESS*", NIL);  //FIXME: Support in reader
  INIT(@"*READTABLE*", readtable);
  //  INIT(@"*STANDARD-INPUT*", );
  INIT(@"*STANDARD-OUTPUT*", outstream);
  //  INIT(@"*TERMINAL-IO*", );
  INIT(@"*TRACE-OUTPUT* ", outstream);

  [vars setObject:NIL forKey:[[MLKPackage findPackage:@"TOILET-SYSTEM"]
                               intern:@"*SYSTEM-INITIALISED-P*"]];
  [vars setObject:[MLKInteger integerWithInt:0]
        forKey:[[MLKPackage findPackage:@"TOILET-SYSTEM"]
                 intern:@"*LOAD-LEVEL*"]];

  global_context = [[self alloc] initWithParent:nil
                                 variables:vars
                                 handlers:nil
                                 restarts:nil
                                 catchTags:nil
                                 activeHandlerEnvironment:nil];

  [MLKRoot registerBuiltins];
}

-(MLKDynamicContext *) initWithParent:(MLKDynamicContext *)aContext
                            variables:(NSDictionary *)vars
                             handlers:(NSDictionary *)handlers
                             restarts:(NSDictionary *)restarts
                            catchTags:(NSSet *)catchTags
             activeHandlerEnvironment:(MLKEnvironment *)handlerEnv;
{
  self = [super init];
  LASSIGN (_parent, (aContext ? aContext : [MLKDynamicContext currentContext]));
  _environment = MAKE_ENVIRONMENT(vars, _parent, _parent->_environment);
  _conditionHandlers = MAKE_ENVIRONMENT(handlers,
                                        _parent,
                                        _parent->_conditionHandlers);
  _restarts = MAKE_ENVIRONMENT(restarts, _parent, _parent->_restarts);
  _catchTags = [[NSSet alloc] initWithSet:catchTags];
  LASSIGN (_activeHandlerEnvironment,
          handlerEnv
          ? (id) handlerEnv
          : (_parent
             ? (id) (_parent->_activeHandlerEnvironment)
             : nil));
  return self;
}

+(MLKDynamicContext *) globalContext
{
  return global_context;
}

-(MLKDynamicContext *) pushContext
{
  [[[NSThread currentThread] threadDictionary] setObject:self
                                               forKey:@"MLKDynamicContext"];
  return self;
}

+(MLKDynamicContext *) currentContext
{
  MLKDynamicContext *context = [[[NSThread currentThread] threadDictionary]
                                 objectForKey:@"MLKDynamicContext"];
  if (context)
    return context;
  else
    return global_context;
}

+(MLKDynamicContext *) popContext
{
  MLKDynamicContext *context = [self currentContext];
  [[[NSThread currentThread] threadDictionary] setObject:context->_parent
                                               forKey:@"MLKDynamicContext"];
  return context;
}

-(MLKEnvironment *) environment
{
  return _environment;
}

-(id) findRestart:(MLKSymbol *)symbol
{
  @try
    {
      return [_restarts valueForSymbol:symbol];
    }
  @catch (MLKUnboundVariableError *e) { }

  return nil;
}

-(id) findHandler:(MLKSymbol *)symbol
{
  @try
    {
      if (_activeHandlerEnvironment)
        return [[_activeHandlerEnvironment parent] valueForSymbol:symbol];
      else
        return [_conditionHandlers valueForSymbol:symbol];
    }
  @catch (MLKUnboundVariableError *e) { }

  return nil;
}

-(BOOL) catchTagIsEstablished:(id)tag
{
  return ([_catchTags containsObject:tag] ||
          (_parent && [_parent catchTagIsEstablished:tag]));
}

-(id) valueForSymbol:(MLKSymbol *)symbol
{
  return [[self environment] valueForSymbol:symbol];
}

-(void) setValue:(id)value forSymbol:(MLKSymbol *)symbol
{
  [[self environment] setValue:value forSymbol:symbol];
}

-(void) addValue:(id)value forSymbol:(MLKSymbol *)symbol
{
  [[self environment] addValue:value forSymbol:symbol];
}

-(void) addBindingForSymbol:(MLKSymbol *)symbol
{
  [[self environment] addBindingForSymbol:symbol];
}

-(MLKBinding *) bindingForSymbol:(MLKSymbol *)symbol
{
  return [[self environment] bindingForSymbol:symbol];
}

-(BOOL) boundp:(MLKSymbol *)symbol
{
  return [[self environment] boundp:symbol];
}

-(void) makunbound:(MLKSymbol *)symbol
{
  [[self environment] makunbound:symbol];
}

-(void) dealloc
{
  LRELEASE (_conditionHandlers);
  LRELEASE (_restarts);
  LRELEASE (_catchTags);
  LRELEASE (_activeHandlerEnvironment);
  LRELEASE (_environment);
  LRELEASE (_parent);
  [super dealloc];
}
@end
