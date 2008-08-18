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

#import "MLKDynamicContext.h"
#import "MLKPackage.h"
#import "MLKSymbol.h"
#import "NSObject-MLKPrinting.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSException.h>
#import <Foundation/NSNull.h>


static NSMutableDictionary *packages = nil;


@implementation MLKPackage
+(void) initialize
{
  MLKPackage *cl, *clUser, *sys, *toilet, *tlUser;

  if (!packages)
    {
      packages = [[NSMutableDictionary alloc] init];

      cl = [MLKPackage packageWithName:@"COMMON-LISP"
                       nicknames:[NSSet setWithObject:@"CL"]];
      clUser = [MLKPackage packageWithName:@"COMMON-LISP-USER"
                           nicknames:[NSSet setWithObject:@"CL-USER"]];;
      sys = [MLKPackage packageWithName:@"TOILET-SYSTEM"
                        nicknames:[NSSet setWithObjects:
                                           @"TL-SYS", @"SYSTEM",
                                           @"SYS", nil]];
      toilet = [MLKPackage packageWithName:@"TOILET-LISP"
                           nicknames:[NSSet setWithObjects:
                                              @"TL", @"TOILET", nil]];
      tlUser = [MLKPackage packageWithName:@"TOILET-LISP-USER"
                           nicknames:[NSSet setWithObjects:
                                              @"TL-USER",
                                              @"TOILET-USER",
                                              nil]];
 
      [MLKPackage packageWithName:@"KEYWORD" nicknames:[NSSet set]];      
    }
  else
    {
      cl = [self findPackage:@"COMMON-LISP"];
      clUser = [self findPackage:@"COMMON-LISP-USER"];
      sys = [self findPackage:@"TOILET-SYSTEM"];
      toilet = [self findPackage:@"TOILET-LISP"];
      tlUser = [self findPackage:@"TOILET-LISP-USER"];
    }

  [cl usePackage:sys];

  [tlUser usePackage:cl];
  [tlUser usePackage:toilet];
  //[tlUser usePackage:clUser];
  
  [toilet usePackage:cl];
  [toilet usePackage:sys];

  [clUser usePackage:cl];
  [clUser usePackage:toilet];
  [clUser usePackage:sys];

  [cl import:nil];
  [cl export:nil];
  [cl export:[cl intern:@"T"]];
  [cl export:[cl intern:@"CATCH"]];
  [cl export:[cl intern:@"THROW"]];
  [cl export:[cl intern:@"IF"]];
  [cl export:[cl intern:@"IN-PACKAGE"]];
  [cl export:[cl intern:@"LET"]];
  [cl export:[cl intern:@"LAMBDA"]];
  [cl export:[cl intern:@"FUNCALL"]];
  [cl export:[cl intern:@"FUNCTION"]];
  [cl export:[cl intern:@"PROGN"]];
  [cl export:[cl intern:@"PROGV"]];
  [cl export:[cl intern:@"SETQ"]];
  [cl export:[cl intern:@"SETF"]];
  [cl export:[cl intern:@"DECLARE"]];
  [cl export:[cl intern:@"QUOTE"]];
  [cl export:[cl intern:@"VALUES"]];
  [cl export:[cl intern:@"EVAL"]];
  [cl export:[cl intern:@"SPECIAL"]];
  [cl export:[cl intern:@"UNWIND-PROTECT"]];
  [cl export:[cl intern:@"MULTIPLE-VALUE-CALL"]];
  [cl export:[cl intern:@"EVAL-WHEN"]];

  [sys export:[sys intern:@"%LAMBDA"]];
  [sys export:[sys intern:@"%FSET"]];
  [sys export:[sys intern:@"%FSETQ"]];
  [sys export:[sys intern:@"%MACROSET"]];
  [sys export:[sys intern:@"%LOOP"]];
  [sys export:[sys intern:@"%FLET"]];
  [sys export:[sys intern:@"%MACROLET"]];
  [sys export:[sys intern:@"%FOREIGN-LAMBDA"]];

  [sys export:[sys intern:@"*SYSTEM-INITIALISED-P*"]];

  [sys export:[sys intern:@"CAR"]];
  [sys export:[sys intern:@"CDR"]];
  [sys export:[sys intern:@"RPLACA"]];
  [sys export:[sys intern:@"RPLACD"]];
  [sys export:[sys intern:@"CONS"]];
  [sys export:[sys intern:@"LOAD"]];
  [sys export:[sys intern:@"EQ"]];
  [sys export:[sys intern:@"ATOM"]];
  [sys export:[sys intern:@"LISTP"]];
  [sys export:[sys intern:@"CONSP"]];
  [sys export:[sys intern:@"SYMBOLP"]];
  [sys export:[sys intern:@"FIXNUMP"]];
  [sys export:[sys intern:@"NULL"]];
  [sys export:[sys intern:@"ADD"]];
  [sys export:[sys intern:@"SUBTRACT"]];
  [sys export:[sys intern:@"MULTIPLY"]];
  [sys export:[sys intern:@"DIVIDE"]];
  [sys export:[sys intern:@"ADD-FIXNUMS"]];
  [sys export:[sys intern:@"SUBTRACT-FIXNUMS"]];
  [sys export:[sys intern:@"MULTIPLY-FIXNUMS"]];
  [sys export:[sys intern:@"DIVIDE-FIXNUMS"]];
  [sys export:[sys intern:@"LIST"]];
  [sys export:[sys intern:@"MACROEXPAND-1"]];
  [sys export:[sys intern:@"MACROEXPAND-ALL"]];
  [sys export:[sys intern:@"EXPORT"]];
  [sys export:[sys intern:@"SHADOW"]];
  [sys export:[sys intern:@"UNEXPORT"]];
  [sys export:[sys intern:@"FIND-PACKAGE"]];
  [sys export:[sys intern:@"STRING"]];
  [sys export:[sys intern:@"GENSYM"]];
  [sys export:[sys intern:@"MAKE-SYMBOL"]];
  [sys export:[sys intern:@"IMPORT"]];
  [sys export:[sys intern:@"INTERN"]];
  [sys export:[sys intern:@"SYMBOL-NAME"]];
  [sys export:[sys intern:@"FIXNUM-EQ"]];
  [sys export:[sys intern:@"DECLARATIONS-AND-DOC-AND-FORMS"]];
  [sys export:[sys intern:@"DECLARATIONS-AND-FORMS"]];
  [sys export:[sys intern:@"COMPILE"]];
  [sys export:[sys intern:@"SET"]];
  [sys export:[sys intern:@"APPLY"]];

  [sys export:[sys intern:@"OBJC-CLASS-OF"]];
  [sys export:[sys intern:@"OBJC-SUBCLASSP"]];
  [sys export:[sys intern:@"FIND-OBJC-CLASS"]];
  [sys export:[sys intern:@"NS-LOG"]];
  [sys export:[sys intern:@"PRIMITIVE-TYPE-OF"]];
  [sys export:[sys intern:@"SEND-BY-NAME"]];

  [cl export:[cl intern:@"*BREAK-ON-SIGNALS*"]];
  [cl export:[cl intern:@"*COMPILE-FILE-PATHNAME*"]];
  [cl export:[cl intern:@"*COMPILE-FILE-TRUENAME*"]];
  [cl export:[cl intern:@"*COMPILE-PRINT*"]];
  [cl export:[cl intern:@"*COMPILE-VERBOSE*"]];
  [cl export:[cl intern:@"*DEBUG-IO*"]];
  [cl export:[cl intern:@"*DEBUGGER-HOOK*"]];
  [cl export:[cl intern:@"*DEFAULT-PATHNAME-DEFAULTS*"]];
  [cl export:[cl intern:@"*ERROR-OUTPUT*"]];
  [cl export:[cl intern:@"*FEATURES*"]];
  [cl export:[cl intern:@"*GENSYM-COUNTER*"]];
  [cl export:[cl intern:@"*LOAD-PATHNAME*"]];
  [cl export:[cl intern:@"*LOAD-PRINT*"]];
  [cl export:[cl intern:@"*LOAD-TRUENAME*"]];
  [cl export:[cl intern:@"*LOAD-VERBOSE*"]];
  [cl export:[cl intern:@"*MACROEXPAND-HOOK*"]];
  [cl export:[cl intern:@"*MODULES*"]];
  [cl export:[cl intern:@"*PACKAGE*"]];
  [cl export:[cl intern:@"*PRINT-ARRAY*"]];
  [cl export:[cl intern:@"*PRINT-BASE*"]];
  [cl export:[cl intern:@"*PRINT-CASE*"]];
  [cl export:[cl intern:@"*PRINT-CIRCLE*"]];
  [cl export:[cl intern:@"*PRINT-ESCAPE*"]];
  [cl export:[cl intern:@"*PRINT-GENSYM*"]];
  [cl export:[cl intern:@"*PRINT-LENGTH*"]];
  [cl export:[cl intern:@"*PRINT-LEVEL*"]];
  [cl export:[cl intern:@"*PRINT-LINES*"]];
  [cl export:[cl intern:@"*PRINT-MISER-WIDTH*"]];
  [cl export:[cl intern:@"*PRINT-PPRINT-DISPATCH*"]];
  [cl export:[cl intern:@"*PRINT-PRETTY*"]];
  [cl export:[cl intern:@"*PRINT-RADIX*"]];
  [cl export:[cl intern:@"*PRINT-READABLY*"]];
  [cl export:[cl intern:@"*PRINT-RIGHT-MARGIN*"]];
  [cl export:[cl intern:@"*QUERY-IO*"]];
  [cl export:[cl intern:@"*RANDOM-STATE*"]];
  [cl export:[cl intern:@"*READ-BASE*"]];
  [cl export:[cl intern:@"*READ-DEFAULT-FLOAT-FORMAT*"]];
  [cl export:[cl intern:@"*READ-EVAL*"]];
  [cl export:[cl intern:@"*READ-SUPPRESS*"]];
  [cl export:[cl intern:@"*READTABLE*"]];
  [cl export:[cl intern:@"*STANDARD-INPUT*"]];
  [cl export:[cl intern:@"*STANDARD-OUTPUT*"]];
  [cl export:[cl intern:@"*TERMINAL-IO*"]];
  [cl export:[cl intern:@"*TRACE-OUTPUT* "]];
}

-(MLKPackage *) initWithName:(NSString *)name
                   nicknames:(NSSet *)nicknames
{
  int i;
  NSArray *e;

  self = [super init];

  [packages setObject:self forKey:name];

  e = [nicknames allObjects];
  for (i = 0; i < [e count]; i++)
    {
      [packages setObject:self forKey:[e objectAtIndex:i]];
    }

  _present_symbols = [[NSMutableSet alloc] init];
  _accessible_symbols = [[NSMutableDictionary alloc] init];
  _exported_symbols = [[NSMutableSet alloc] init];
  _shadowing_symbols = [[NSMutableSet alloc] init];
  _nicknames = [[NSMutableSet alloc] initWithSet:nicknames];
  _used_packages = [[NSMutableArray alloc] init];
  _using_packages = [[NSMutableArray alloc] init];
  LASSIGN (_name, name);

  return self;
}

+(MLKPackage *) packageWithName:(NSString *)name
                      nicknames:(NSSet *)nicknames
{
  return LAUTORELEASE ([[self alloc] initWithName:name nicknames:nicknames]);
}

+(MLKPackage *) findPackage:(NSString *)name
{
  return [packages objectForKey:name];
}

-(void) usePackage:(MLKPackage *)package
{
  int i;
  NSArray *symbols;

  symbols = [[package exportedSymbols] allObjects];

  for (i = 0; i < [symbols count]; i++)
    {
      [self inherit:[symbols objectAtIndex:i]];
    }

  [_used_packages addObject:package];
  [package->_using_packages addObject:self];
}

-(void) unusePackage:(MLKPackage *)package
{
  int i;
  NSArray *symbols;

  symbols = [[package exportedSymbols] allObjects];

  for (i = 0; i < [symbols count]; i++)
    {
      [self uninherit:[symbols objectAtIndex:i]];
    }

  [_used_packages removeObject:package];
  [package->_using_packages removeObject:self];
}

-(void) import:(MLKSymbol *)symbol
{
  id old_symbol;
  NSString *name;

  name = symbol ? [symbol name] : (NSString *)@"NIL";
  symbol = symbol ? (id)symbol : (id)[NSNull null];

  if ((old_symbol = [_accessible_symbols objectForKey:name]))
    {
      if (old_symbol != symbol)
        [NSException
          raise:@"MLKSymbolConflictError"
          format:@"Imported symbol %@ conflicts with accessible symbol %@ in package %@.",
                 MLKPrintToString(symbol),
                 MLKPrintToString(old_symbol),
                 MLKPrintToString(self)];
    }

  [_accessible_symbols setObject:symbol forKey:name];
  [_present_symbols addObject:symbol];

  if ([_name isEqual:@"KEYWORD"])
    {
      // Make keyword symbols self-evaluate.
      [[MLKDynamicContext globalContext] addValue:symbol forSymbol:symbol];

      // Make them external as well.
      [self export:symbol];

      // FIXME: Should finally make them constant, see CLHS 10.2, Type KEYWORD.
    }
}

-(void) inherit:(MLKSymbol *)symbol
{
  id old_symbol;
  NSString *name;

  name = symbol ? [symbol name] : (NSString *)@"NIL";
  symbol = nullify (symbol);
  old_symbol = [_accessible_symbols objectForKey:name];

  if (old_symbol && [_shadowing_symbols containsObject:old_symbol])
    return;

  if (old_symbol && old_symbol != symbol)
    [NSException
      raise:@"MLKSymbolConflictError"
      format:@"Inherited symbol %@ conflicts with accessible symbol %@ in package %@.",
             MLKPrintToString(symbol),
             MLKPrintToString(old_symbol),
             MLKPrintToString(self)];

  [_accessible_symbols setObject:symbol forKey:name];
  
  if ([_name isEqual:@"KEYWORD"])
    {
      // Make keyword symbols self-evaluate.
      [[MLKDynamicContext globalContext] addValue:symbol forSymbol:symbol];

      // Make them external as well.
      [self export:symbol];

      // FIXME: Should finally make them constant, see CLHS 10.2, Type KEYWORD.
    }
}

-(void) uninherit:(MLKSymbol *)symbol
{
  NSString *name;

  name = symbol ? [symbol name] : (NSString *)@"NIL";

  if ([_accessible_symbols objectForKey:name] == symbol
      && ![_present_symbols containsObject:symbol])
    [_accessible_symbols removeObjectForKey:name];
}

-(void) export:(MLKSymbol *)symbol
{
  int i;
  NSString *name;

  name = symbol ? [symbol name] : (NSString *)@"NIL";
  symbol = nullify (symbol);

  for (i = 0; i < [_using_packages count]; i++)
    {
      MLKPackage *package = [_using_packages objectAtIndex:i];
      id old_symbol = [package->_accessible_symbols objectForKey:name];

      if (old_symbol
          && (old_symbol != symbol)
          && ![_shadowing_symbols containsObject:old_symbol])
        [NSException
          raise:@"MLKSymbolConflictError"
          format:@"Exported symbol %@ conflicts with accessible symbol %@ in package %@.",
                 MLKPrintToString(symbol),
                 MLKPrintToString(old_symbol),
                 MLKPrintToString(package)];
    }

  for (i = 0; i < [_using_packages count]; i++)
    {
      [[_using_packages objectAtIndex:i]
        inherit:(symbol == (id)[NSNull null] ? nil : (id)symbol)];
    }

  [_exported_symbols addObject:symbol];
}

-(void) unexport:(MLKSymbol *)symbol
{
  int i;

  symbol = nullify (symbol);

  [_exported_symbols removeObject:symbol];

  for (i = 0; i < [_using_packages count]; i++)
    {
      [[_using_packages objectAtIndex:i] uninherit:symbol];
    }
}

-(void) shadow:(NSString *)symbolName
{
  MLKSymbol *symbol;

  symbol = [_accessible_symbols objectForKey:symbolName];
  if (!symbol || ![_present_symbols containsObject:symbol])
    {
      //NSLog (@"Shadowing %@", symbol);
      symbol = [MLKSymbol symbolWithName:symbolName package:self];
      [_accessible_symbols setObject:symbol forKey:symbolName];
      [_present_symbols addObject:symbol];
    }
  [_shadowing_symbols addObject:symbol];
}

-(void) unintern:(MLKSymbol *)aSymbol
{
  // FIXME: Check for conflicts.
  if ([_present_symbols containsObject:aSymbol])
    {
      [_present_symbols removeObject:aSymbol];
      [_accessible_symbols removeObjectForKey:[aSymbol name]];
      [_shadowing_symbols removeObject:aSymbol];

      if (self == [aSymbol homePackage])
        [aSymbol setHomePackage:nil];
    }
}

-(MLKSymbol *) intern:(NSString *)symbolName
{
  MLKSymbol *symbol;
  if ((symbol = [_accessible_symbols objectForKey:symbolName]))
    return (symbol == (id)[NSNull null] ? nil : (id)symbol);
  else
    {
      //NSLog (@"Interning %@", symbolName);
      MLKSymbol *symbol = [[MLKSymbol alloc] initWithName:symbolName
                                             package:self];
      [self import:symbol];
      return symbol;
    }
}

-(MLKSymbol *) findSymbol:(NSString *)symbolName
{
  MLKSymbol *symbol;
  if ((symbol = [_accessible_symbols objectForKey:symbolName]))
    return (symbol == (id)[NSNull null] ? nil : (id)symbol);
  else
    [NSException raise:@"MLKNoSuchSymbolError"
                 format:@"The package %@ does not contain a symbol named %@.",
                        self,
                        symbolName];

  return nil;
}

-(NSString *) name
{
  return _name;
}

-(NSSet *) nicknames
{
  return _nicknames;
}

-(NSSet *) exportedSymbols
{
  return _exported_symbols;
}

-(NSSet *) shadowingSymbols
{
  return _shadowing_symbols;
}

-(NSSet *) allSymbols
{
  return [NSSet setWithArray:[_accessible_symbols allValues]];
}

-(NSArray *) usedPackages
{
  return _used_packages;
}

-(NSArray *) usingPackages
{
  return _using_packages;
}

-(NSString *) descriptionForLisp
{
  return [NSString stringWithFormat:@"#<Package %@>", MLKPrintToString ([self name])];
}

-(void) dealloc
{
  LRELEASE (_present_symbols);
  LRELEASE (_accessible_symbols);
  LRELEASE (_exported_symbols);
  LRELEASE (_shadowing_symbols);
  LRELEASE (_nicknames);
  LRELEASE (_used_packages);
  LRELEASE (_using_packages);
  LRELEASE (_name);
  [super dealloc];
}
@end
