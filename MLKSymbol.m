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

#import "MLKSymbol.h"

#import <Foundation/NSString.h>


@implementation MLKSymbol
-(MLKSymbol *) initWithName:(id)aName package:(id)aPackage
{
  self = [super init];
  ASSIGN (name, aName);
  ASSIGN (homePackage, aPackage);
  return self;
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

-(NSString *)descriptionForLisp
{
  // FIXME: This is wrong in more than one way.
  return [NSString stringWithFormat:@"|%@|", name];
}

-(void) dealloc
{
  RELEASE (name);
  RELEASE (homePackage);
  [super dealloc];
}
@end
