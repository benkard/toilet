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

#import "MLKDynamicContext.h"
#import "MLKPackage.h"
#import "MLKReader.h"
#import "MLKReadtable.h"
#import "MLKSymbol.h"
#import "runtime-compatibility.h"
#import "util.h"

#import <Foundation/NSException.h>
#import <Foundation/NSString.h>


@implementation MLKSymbol
-(MLKSymbol *) initWithName:(id)aName package:(id)aPackage
{
  self = [super init];
  ASSIGN (name, aName);
  ASSIGN (homePackage, aPackage);
  real_identity = nil;
  return self;
}

+(MLKSymbol *) symbolWithName:(id)aName package:(id)aPackage
{
  return AUTORELEASE ([[self alloc] initWithName:aName package:aPackage]);
}

-(id) copyWithZone:(NSZone *)zone
{
  MLKSymbol *copy = [MLKSymbol allocWithZone:zone];
  ASSIGN (copy->name, name);
  ASSIGN (copy->homePackage, homePackage);
  if (real_identity)
    ASSIGN (copy->real_identity, real_identity);
  else
    ASSIGN (copy->real_identity, self);
  return copy;
}

-(NSString *) name
{
  return name;
}

-(MLKPackage *) homePackage
{
  return homePackage;
}

-(void) setHomePackage:(MLKPackage *)aPackage
{
  ASSIGN (homePackage, aPackage);
}

-(NSString *) descriptionForLisp
{
  // NOTE: Need to take *PRINT-GENSYM* into account.
  //
  // FIXME: This is wrong in more than one way.
  MLKDynamicContext *dynctx = [MLKDynamicContext currentContext];
  MLKPackage *cl = [MLKPackage findPackage:@"COMMON-LISP"];
  MLKPackage *currentPackage = [dynctx valueForSymbol:[cl intern:@"*PACKAGE*"]];
  MLKReadtable *readtable = [dynctx valueForSymbol:[cl intern:@"*READTABLE*"]];
  BOOL accessible;
  NSString *packagePrefix;
  NSString *printName;
  BOOL escaped;
  int i;
  int base = [[dynctx valueForSymbol:[cl intern:@"*PRINT-BASE*"]] intValue];

  escaped = NO;
  
  if ([MLKReader isPotentialNumber:name readtable:readtable base:base])
    escaped = YES;

  if ([name length] == 0)
    escaped = YES;

  for (i = 0; i < [name length]; i++)
    {
      unichar ch = [name characterAtIndex:i];
      escaped =
        escaped                                                           \
        || ![readtable isConstituentCharacter:ch]                         \
        || ![[[NSString stringWithFormat:@"%C", ch] uppercaseString]
              isEqualToString:[NSString stringWithFormat:@"%C", ch]];
    }

  if (escaped)
    {
      NSMutableString *tmp = [NSMutableString stringWithString:name];
      [tmp replaceOccurrencesOfString:@"\\"
           withString:@"\\\\"
           options:NSLiteralSearch
           range:NSMakeRange(0, [tmp length])];
      [tmp replaceOccurrencesOfString:@"|"
           withString:@"\\|"
           options:NSLiteralSearch
           range:NSMakeRange(0, [tmp length])];
      printName = [NSString stringWithFormat:@"|%@|", tmp];
    }
  else
    printName = name;

  NS_DURING
    {
      if ([currentPackage findSymbol:name] == self)
        accessible = YES;
      else
        accessible = NO;
    }
  NS_HANDLER
    {
      if ([[localException name] isEqualToString:@"MLKNoSuchSymbolError"])
        accessible = NO;
      else
        [localException raise];
    }
  NS_ENDHANDLER;

  if (homePackage == [MLKPackage findPackage:@"KEYWORD"])
    packagePrefix = @":";
  else if (!homePackage)
    packagePrefix = @"#:";
  else if (accessible)
    packagePrefix = [NSString string];
  else
    packagePrefix = [NSString stringWithFormat:@"|%@|%s",
                              [homePackage name],
                              ([[homePackage exportedSymbols] containsObject:self]
                               ? ":"
                               : "::")];

  return [NSString stringWithFormat:@"%@%@", packagePrefix, printName];
}

-(NSString *) description
{
  return MLKPrintToString(self);
}

-(BOOL) isEqual:(id)object
{
  if (object == self)
    return YES;

  if (!([object isKindOfClass:[MLKSymbol class]]))
    return NO;

  return ((((MLKSymbol *)object)->real_identity
           ? ((MLKSymbol *)object)->real_identity
           : (MLKSymbol *)object)
          == ((self->real_identity != nil
               ? self->real_identity
               : self)));
}

-(unsigned) hash
{
  if (real_identity)
    return [real_identity hash];
  else
    return [super hash];
}

-(void) dealloc
{
  RELEASE (name);
  RELEASE (homePackage);
  [super dealloc];
}
@end
