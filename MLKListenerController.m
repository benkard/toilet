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

#import "MLKDynamicContext.h"
#import "MLKPackage.h"
#import "MLKReader.h"
#import "util.h"
#import "special-symbols.h"

@implementation MLKListenerController
+ (void)initialize
{
  ensure_symbols();
}

- (IBAction)submit:(id)sender
{
  id object;
  NSDictionary *attrs;
  NSString *input = [inputField stringValue];
  MLKPackage *package;
  MLKDynamicContext *newctx;

  [submitButton setEnabled:NO];

  NS_DURING
    {
      object = [MLKReader readFromString:input];
    }
  NS_HANDLER
    {
      // A parsing error.  Beep and let the user try again.
      // XXX Maybe the status line could be made to provide more information on the error.
      NSBeep();
      [submitButton setEnabled:YES];
      [inputField selectText:self];
      return;
    }
  NS_ENDHANDLER;

  [inputField setStringValue:@""];
  [inputField selectText:self];

  package = [[MLKDynamicContext currentContext]
             valueForSymbol:[[MLKPackage findPackage:@"COMMON-LISP"]
                             intern:@"*PACKAGE*"]];

  NSMutableAttributedString *text = [outputTextView textStorage];
  [text beginEditing];

  attrs = [NSDictionary dictionaryWithObjectsAndKeys:
    [NSColor blueColor], NSForegroundColorAttributeName, nil];
  NSString *barePrompt = [NSString stringWithFormat:@"%@> ", [package name]];
  NSAttributedString *prompt =
    LAUTORELEASE ([[NSAttributedString alloc] initWithString:barePrompt
                                                  attributes:attrs]);
  [text appendAttributedString:prompt];

  attrs = [NSDictionary dictionaryWithObjectsAndKeys:
    [NSColor blackColor], NSForegroundColorAttributeName, nil];
  NSAttributedString *inputFeedback =
    LAUTORELEASE ([[NSAttributedString alloc] initWithString:input attributes:attrs]);
  [text appendAttributedString:inputFeedback];

  [[text mutableString] appendString:@"\n"];
  [text endEditing];

  [statusText setStringValue:@"Compiling and executing."];
  NS_DURING
    {
      NSDictionary *vars = [NSDictionary dictionaryWithObjectsAndKeys:
                                           self, QUERY_IO,
                                           self, ERROR_OUTPUT,
                                           self, STANDARD_OUTPUT,
                                           self, TERMINAL_IO,
                                           self, TRACE_OUTPUT,
                                           self, DEBUG_IO,
                                           nil];
      MLKDynamicContext *ctx = [MLKDynamicContext currentContext];
      newctx = [[MLKDynamicContext alloc] initWithParent:ctx
                                          variables:vars
                                          handlers:nil
                                          restarts:nil
                                          catchTags:nil
                                          activeHandlerEnvironment:nil];
      [newctx pushContext];

      // ...
    }
  NS_HANDLER
    {
      // ...
    }
  NS_ENDHANDLER;

  [MLKDynamicContext popContext];
  LDESTROY (newctx);
  [statusText setStringValue:@"Ready."];

  [text beginEditing];
  attrs = [NSDictionary dictionaryWithObjectsAndKeys:
    [NSColor purpleColor], NSForegroundColorAttributeName, nil];
  NSAttributedString *response =
    LAUTORELEASE ([[NSAttributedString alloc] initWithString:MLKPrintToString(object)
                                                  attributes:attrs]);
  [text appendAttributedString:response];

  [[text mutableString] appendString:@"\n"];

  [text endEditing];

  [submitButton setEnabled:YES];
}

- (void)writeChar:(unichar)ch
{
  [self writeString:[NSString stringWithFormat:@"%C", ch]];
}

- (void)writeString:(NSString *)string
{
  NSDictionary *attrs = [NSDictionary dictionaryWithObjectsAndKeys:
                                        [NSColor brownColor],
                                        NSForegroundColorAttributeName,
                                        nil];
  NSAttributedString *output =
    LAUTORELEASE ([[NSAttributedString alloc] initWithString:string
                                                  attributes:attrs]);
  [[outputTextView textStorage] appendAttributedString:output];
}
@end
