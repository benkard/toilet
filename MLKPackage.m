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
#import "MLKError.h"
#import "runtime-compatibility.h"

#import <Foundation/NSDictionary.h>
#import <Foundation/NSSet.h>
#import <Foundation/NSString.h>
#import <Foundation/NSArray.h>


static NSMutableDictionary *packages = nil;


@implementation MLKPackage
+(void) initialize
{
  packages = [[NSMutableDictionary alloc] init];
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

  _symbols = [[NSMutableDictionary alloc] init];
  _exported_symbols = [[NSMutableSet alloc] init];
  _shadowed_symbols = [[NSMutableSet alloc] init];
  _nicknames = [[NSMutableSet alloc] initWithSet:nicknames];
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

-(void) usePackage:(MLKPackage *)aPackage
{
  int i;
  NSArray *symbols;

  symbols = [[aPackage allSymbols] allObjects];

  for (i = 0; i < [symbols count]; i++)
    [self import:[symbols objectAtIndex:i]];
}

-(void) import:(MLKSymbol *)aSymbol
{
  // FIXME: Check for conflicts.

  // FIXME: What to do about exported and shadowed symbols that conflict
  // with the new one?
  [_symbols setObject:aSymbol forKey:[aSymbol name]];
}

-(void) export:(MLKSymbol *)aSymbol
{
  [_exported_symbols addObject:aSymbol];
}

-(void) shadow:(MLKSymbol *)aSymbol
{
  [_shadowed_symbols addObject:aSymbol];
}

-(void) unintern:(MLKSymbol *)aSymbol
{
  [_symbols removeObjectForKey:[aSymbol name]];
}

-(MLKSymbol *) intern:(NSString *)symbolName
{
  if ([[_symbols allKeys] containsObject:symbolName])
    return [_symbols objectForKey:symbolName];
  else
    {
      MLKSymbol *symbol = [[MLKSymbol alloc] initWithName:symbolName
                                             package:self];
      [_symbols setObject:symbol forKey:symbolName];
      return symbol;
    }
}

-(MLKSymbol *) findSymbol:(NSString *)symbolName
{
  if ([[_symbols allKeys] containsObject:symbolName])
    return [_symbols objectForKey:symbolName];
  else
    [[MLKError errorWithMessage:@"Symbol not found."] raise];

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

-(NSSet *) shadowedSymbols
{
  return _shadowed_symbols;
}

-(NSSet *) allSymbols
{
  return [NSSet setWithArray:[_symbols allValues]];
}
@end
