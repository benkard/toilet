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

#import "MLKPackage.h"
#import "MLKSymbol.h"
#import "NSObject-MLKPrinting.h"
#import "runtime-compatibility.h"

#import <Foundation/NSException.h>
#import <Foundation/NSNull.h>


static NSMutableDictionary *packages = nil;


@implementation MLKPackage
+(void) initialize
{
  packages = [[NSMutableDictionary alloc] init];
  
  MLKPackage *cl = [MLKPackage packageWithName:@"COMMON-LISP"
                               nicknames:[NSSet setWithObject:@"CL"]];
  MLKPackage *clUser = [MLKPackage packageWithName:@"COMMON-LISP-USER"
                                   nicknames:[NSSet setWithObject:@"CL-USER"]];;
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

  [MLKPackage packageWithName:@"KEYWORD" nicknames:[NSSet set]];

  [tlUser usePackage:cl];
  [tlUser usePackage:toilet];

  [clUser usePackage:cl];
  [clUser usePackage:toilet];
  [clUser usePackage:sys];

  [cl import:nil];
  [cl export:nil];
  [cl export:[cl intern:@"T"]];
  [cl export:[cl intern:@"IF"]];
  [cl export:[cl intern:@"LET"]];
  [cl export:[cl intern:@"LAMBDA"]];
  [cl export:[cl intern:@"FUNCALL"]];
  [cl export:[cl intern:@"PROGN"]];
  [cl export:[cl intern:@"APPLY"]];
  [cl export:[cl intern:@"PROGV"]];
  [cl export:[cl intern:@"SETQ"]];
  [cl export:[cl intern:@"DECLARE"]];
  [cl export:[cl intern:@"QUOTE"]];

  [sys export:[sys intern:@"%DEFMACRO"]];
  [sys export:[sys intern:@"%LAMBDA"]];

  [tlUser usePackage:clUser];
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
  ASSIGN (_name, name);

  return self;
}

+(MLKPackage *) packageWithName:(NSString *)name
                      nicknames:(NSSet *)nicknames
{
  return AUTORELEASE ([[self alloc] initWithName:name nicknames:nicknames]);
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
          format:@"Imported symbol %@ conflicts with accessible symbol %@.",
                 [symbol descriptionForLisp],
                 [old_symbol descriptionForLisp]];
    }

  [_accessible_symbols setObject:symbol forKey:name];
  [_present_symbols addObject:symbol];
}

-(void) inherit:(MLKSymbol *)symbol
{
  id old_symbol;
  NSString *name;

  name = symbol ? [symbol name] : (NSString *)@"NIL";
  symbol = symbol ? (id)symbol : (id)[NSNull null];

  if ((old_symbol = [_accessible_symbols objectForKey:name])
      && old_symbol != symbol
      && ![_shadowing_symbols containsObject:old_symbol])
    [NSException
      raise:@"MLKSymbolConflictError"
      format:@"Inherited symbol %@ conflicts with accessible symbol %@.",
             [symbol descriptionForLisp],
             [old_symbol descriptionForLisp]];

  [_accessible_symbols setObject:symbol forKey:name];
}

-(void) uninherit:(MLKSymbol *)symbol
{
  NSString *name;

  name = symbol ? [symbol name] : (NSString *)@"NIL";
  symbol = symbol ? (id)symbol : (id)[NSNull null];

  if (![_present_symbols containsObject:symbol])
    [_accessible_symbols removeObjectForKey:name];
}

-(void) export:(MLKSymbol *)symbol
{
  int i;
  NSString *name;

  name = symbol ? [symbol name] : (NSString *)@"NIL";
  symbol = symbol ? (id)symbol : (id)[NSNull null];

  for (i = 0; i < [_using_packages count]; i++)
    {
      MLKPackage *package = [_using_packages objectAtIndex:i];
      id old_symbol = [package->_accessible_symbols objectForKey:name];

      if (old_symbol
          && (old_symbol != symbol)
          && ![_shadowing_symbols containsObject:old_symbol])
        [NSException
          raise:@"MLKSymbolConflictError"
          format:@"Inherited symbol %@ conflicts with accessible symbol %@ in package %@.",
                 [symbol descriptionForLisp],
                 [old_symbol descriptionForLisp],
                 [package descriptionForLisp]];
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

  symbol = symbol ? (id)symbol : (id)[NSNull null];

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
  if (!symbol)
    {
      symbol = [MLKSymbol symbolWithName:symbolName package:self];
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
    }
}

-(MLKSymbol *) intern:(NSString *)symbolName
{
  MLKSymbol *symbol;
  if ((symbol = [_accessible_symbols objectForKey:symbolName]))
    return (symbol == (id)[NSNull null] ? nil : (id)symbol);
  else
    {
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

-(void) dealloc
{
  RELEASE (_present_symbols);
  RELEASE (_accessible_symbols);
  RELEASE (_exported_symbols);
  RELEASE (_shadowing_symbols);
  RELEASE (_nicknames);
  RELEASE (_used_packages);
  RELEASE (_using_packages);
  RELEASE (_name);
  [super dealloc];
}
@end
