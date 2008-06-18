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

#import "MLKLispValue.h"

#import <Foundation/NSObject.h>

@class MLKPackage;


@interface MLKSymbol : MLKLispValue <NSCopying>
{
  NSString *name;
  MLKPackage *homePackage;
  MLKSymbol *real_identity;
}

-(MLKSymbol *) initWithName:(id)aName package:(id)aPackage;

-(NSString *) name;
-(MLKPackage *) homePackage;
-(void) setHomePackage:(MLKPackage *)aPackage;

-(NSString *)descriptionForLisp;

// PLEASE DO NOT USE THIS.
//
// Symbols should never be copied.  MLKSymbol needs to implement
// NSCopying as well as suitable version of isEqual: so that symbols can
// be used as dictionary keys.  Don't call -copy on a symbol manually,
// please.
-(id) copyWithZone:(NSZone *)zone;

// PLEASE DO NOT USE THIS.
//
// It uses an ugly hack for determining symbol identity in the face of
// copying.  (The hack is called real_identity and it's an ivar of this
// class.)  See the comment above copyWithZone: for why it even exists.
-(BOOL) isEqual:(id)object;
-(unsigned) hash;

-(void) dealloc;
@end
