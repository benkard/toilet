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

#import "MLKListenerController.h"
#import "MLKReader.h"

@implementation MLKListenerController
- (IBAction)submit:(id)sender
{
  id object;
  NSString *input = [inputField stringValue];

  NS_DURING
    {
      object = [MLKReader readFromString:input];
    }
  NS_HANDLER
    {
      // A parsing error.  Beep and let the user try again.
      // XXX Maybe the status line could be made to provide more information on the error.
      NSBeep();
      [inputField selectText:self];
      return;
    }
  NS_ENDHANDLER;

  [inputField setStringValue:@""];
}
@end
