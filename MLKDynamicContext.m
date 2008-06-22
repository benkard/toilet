/* -*- mode: objc; coding: utf-8 -*- */
/* Étoilisp/Mulklisp, a Common Lisp subset for the Étoilé runtime.
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

#import "MLKCons.h"
#import "MLKDynamicContext.h"
#import "MLKEnvironment.h"
#import "MLKLinkedList.h"
#import "MLKPackage.h"
#import "MLKParenReader.h"
#import "MLKReadtable.h"
#import "MLKSymbol.h"
#import "MLKInteger.h"
#import "runtime-compatibility.h"


#define MAKE_ENVIRONMENT(variable, parent, parent_member)               \
  (variable                                                             \
   ? (id) [[MLKEnvironment alloc]                                       \
            initWithParent:(parent                                      \
                            ? (id) parent_member                        \
                            : nil)                                      \
                  bindings:vars]                                        \
   : (id) (parent ? (id) RETAIN (parent_member) : nil));


static MLKDynamicContext *global_context;


@implementation MLKDynamicContext
+(void) initialize
{
  NSMutableDictionary *vars = [NSMutableDictionary dictionaryWithCapacity:64];
  MLKPackage *cl = [MLKPackage packageWithName:@"COMMON-LISP"
                               nicknames:[NSSet setWithObject:@"CL"]];
  MLKPackage *clUser = [MLKPackage packageWithName:@"COMMON-LISP-USER"
                                   nicknames:[NSSet setWithObject:@"CL-USER"]];
  MLKPackage *keyword = [MLKPackage packageWithName:@"KEYWORD"
                                    nicknames:[NSSet set]];
  MLKPackage *sys = [MLKPackage packageWithName:@"TOILET-SYSTEM"
                                nicknames:[NSSet setWithObjects:
                                                   @"TL-SYS", nil]];
  MLKPackage *toilet = [MLKPackage packageWithName:@"TOILET-LISP"
                                   nicknames:[NSSet setWithObjects:
                                                      @"TL", @"TOILET", nil]];
  MLKPackage *tlUser = [MLKPackage packageWithName:@"TOILET-LISP-USER"
                                   nicknames:[NSSet setWithObjects:
                                                      @"TL-USER",
                                                      @"TOILET-USER",
                                                      nil]];
  MLKSymbol *t = [cl intern:@"T"];
  MLKReadtable *readtable = [[MLKReadtable alloc] init];
  unichar ch;

  id NIL = [NSNull null];

  [sys intern:@"%DEFMACRO"];
  [tlUser usePackage:clUser];
  [toilet import:nil];

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

  //  [readtable setSyntaxType:TERMINATING_MACRO forCharacter:';'];
  //  [readtable setSyntaxType:TERMINATING_MACRO forCharacter:'"'];
  //  [readtable setSyntaxType:NONTERMINATING_MACRO forCharacter:'#'];
  //  [readtable setSyntaxType:TERMINATING_MACRO forCharacter:'\''];
  //  [readtable setSyntaxType:TERMINATING_MACRO forCharacter:'`'];
  //  [readtable setSyntaxType:TERMINATING_MACRO forCharacter:','];

  [readtable setSyntaxType:SINGLE_ESCAPE forCharacter:'\\'];
  
  [readtable setSyntaxType:TERMINATING_MACRO forCharacter:'('];
  [readtable setMacroFunction:[[MLKParenReader alloc] init]
             forCharacter:'('];
  [readtable setSyntaxType:TERMINATING_MACRO forCharacter:')'];

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

  INIT(@"*BREAK-ON-SIGNALS*", NIL);
  INIT(@"*COMPILE-FILE-PATHNAME*", NIL);
  INIT(@"*COMPILE-FILE-TRUENAME*", NIL);
  INIT(@"*COMPILE-PRINT*", NIL);
  INIT(@"*COMPILE-VERBOSE*", t);
  //  INIT(@"*DEBUG-IO*", );
  INIT(@"*DEBUGGER-HOOK*", NIL);
  //  INIT(@"*DEFAULT-PATHNAME-DEFAULTS*", );
  //  INIT(@"*ERROR-OUTPUT*", );
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
  //  INIT(@"*STANDARD-OUTPUT*", );
  //  INIT(@"*TERMINAL-IO*", );
  //  INIT(@"*TRACE-OUTPUT* ", );

  global_context = [[self alloc] initWithParent:nil
                                 variables:vars
                                 handlers:nil
                                 restarts:nil
                                 catchTags:nil
                                 activeHandlerEnvironment:nil];
}

-(MLKDynamicContext *) initWithParent:(MLKDynamicContext *)aContext
                            variables:(NSDictionary *)vars
                             handlers:(NSDictionary *)handlers
                             restarts:(NSDictionary *)restarts
                            catchTags:(NSDictionary *)catchTags
             activeHandlerEnvironment:(MLKEnvironment *)handlerEnv;
{
  self = [super init];
  ASSIGN (_parent, (aContext ? aContext : [MLKDynamicContext currentContext]));
  _environment = MAKE_ENVIRONMENT(vars, _parent, _parent->_environment);
  _conditionHandlers = MAKE_ENVIRONMENT(handlers,
                                        _parent,
                                        _parent->_conditionHandlers);
  _restarts = MAKE_ENVIRONMENT(restarts, _parent, _parent->_restarts);
  _catchTags = MAKE_ENVIRONMENT(catchTags, _parent, _parent->_catchTags);
  ASSIGN (_activeHandlerEnvironment,
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
  NS_DURING
    {
      return [_restarts valueForBinding:symbol];
    }
  NS_HANDLER
    {
      if ([[localException name] isEqualToString: @"MLKUndefinedVariableException"])
        NS_VALUERETURN (nil, id);
      else
        [localException raise];
    }
  NS_ENDHANDLER;

  return nil;
}

-(id) findHandler:(MLKSymbol *)symbol
{
  NS_DURING
    {
      if (_activeHandlerEnvironment)
        return [[_activeHandlerEnvironment parent] valueForBinding:symbol];
      else
        return [_conditionHandlers valueForBinding:symbol];
    }
  NS_HANDLER
    {
      if ([[localException name] isEqualToString: @"MLKUndefinedVariableException"])
        NS_VALUERETURN (nil, id);
      else
        [localException raise];
    }
  NS_ENDHANDLER;

  return nil;
}

-(id) findCatchTag:(MLKSymbol *)symbol
{
  NS_DURING
    {
      return [_catchTags valueForBinding:symbol];
    }
  NS_HANDLER
    {
      if ([[localException name] isEqualToString: @"MLKUndefinedVariableException"])
        NS_VALUERETURN (nil, id);
      else
        [localException raise];
    }
  NS_ENDHANDLER;

  return nil;
}

-(id) valueForBinding:(MLKSymbol *)symbol
{
  return [[self environment] valueForBinding:symbol];
}

-(void) setValue:(id)value forBinding:(MLKSymbol *)symbol
{
  [[self environment] setValue:value forBinding:symbol];
}

-(void) addValue:(id)value forBinding:(MLKSymbol *)symbol
{
  [[self environment] addValue:value forBinding:symbol];
}

-(void) addBinding:(MLKSymbol *)symbol
{
  [[self environment] addBinding:symbol];
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
  RELEASE (_conditionHandlers);
  RELEASE (_restarts);
  RELEASE (_catchTags);
  RELEASE (_activeHandlerEnvironment);
  RELEASE (_environment);
  RELEASE (_parent);
  [super dealloc];
}
@end
