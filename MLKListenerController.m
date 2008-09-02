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
#import "MLKLexicalContext.h"
#import "MLKLexicalEnvironment.h"
#import "MLKInterpreter.h"
#import "MLKPackage.h"
#import "MLKReader.h"
#import "util.h"
#import "special-symbols.h"

@implementation MLKListenerController
+ (void)initialize
{
  ensure_symbols();
}

- (void) initialiseInterpreter
{
  [inputField setStringValue:@"(require \"init.lisp\")"];
  [self submit:self];
}

- (IBAction)submit:(id)sender
{
  MLKPackage *package;
  id object;
  NSDictionary *attrs;
  NSString *input = [inputField stringValue];
  float originalScrollPosition = [[[outputTextView enclosingScrollView]
                                    verticalScroller]
                                   floatValue];

  [submitButton setEnabled:NO];

  @try
    {
      object = [MLKReader readFromString:input];
    }
  @catch (NSException *e)
    {
      // A parsing error.  Beep and let the user try again.
      // XXX Maybe the status line could be made to provide more information on the error.
      NSBeep();
      [submitButton setEnabled:YES];
      [inputField selectText:self];
      return;
    }

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

  if (originalScrollPosition == 1.0)
    {
      NSRange range = NSMakeRange ([text length], 0);
      [outputTextView scrollRangeToVisible:range];
    }

  [NSThread detachNewThreadSelector:@selector(evalObject:)
                           toTarget:self
                         withObject:nullify(object)];
}

- (void)evalObject:(id)object
{
  MLKDynamicContext *newctx;
  NSDictionary *attrs;
  NSMutableAttributedString *text = [outputTextView textStorage];
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  BOOL waitp = NO;
   float originalScrollPosition = [[[outputTextView enclosingScrollView]
                                     verticalScroller]
                                    floatValue];

  object = denullify(object);

  @try
    {
      int i;
      NSArray *results;
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

      results = [MLKInterpreter eval:object
                    inLexicalContext:[MLKLexicalContext globalContext]
                     withEnvironment:[MLKLexicalEnvironment globalEnvironment]];

      [text performSelectorOnMainThread:@selector(beginEditing)
            withObject:nil
            waitUntilDone:waitp];

      for (i = 0; i < [results count]; i++)
        {
          id result = denullify ([results objectAtIndex:i]);

          //[text beginEditing];
          attrs = [NSDictionary dictionaryWithObjectsAndKeys:
            [NSColor purpleColor], NSForegroundColorAttributeName, nil];
          NSAttributedString *response =
            LAUTORELEASE ([[NSAttributedString alloc] initWithString:MLKPrintToString(result)
                                                          attributes:attrs]);
          [text performSelectorOnMainThread:@selector(appendAttributedString:)
                withObject:response
                waitUntilDone:waitp];
          [[text mutableString]
            performSelectorOnMainThread:@selector(appendString:)
            withObject:@"\n"
            waitUntilDone:waitp];
        }      
    }
  @catch (NSException *localException)
    {
      NSString *bare_msg = [NSString stringWithFormat:
               @"Caught an unhandled exception.\nName: %s\nReason: %s\n",
        [[localException name] UTF8String],
        [[localException reason] UTF8String]];

      [text performSelectorOnMainThread:@selector(beginEditing)
            withObject:nil
            waitUntilDone:waitp];

      attrs = [NSDictionary dictionaryWithObjectsAndKeys:
        [NSColor redColor], NSForegroundColorAttributeName, nil];
      NSAttributedString *response =
        LAUTORELEASE ([[NSAttributedString alloc] initWithString:bare_msg
                                                      attributes:attrs]);
      [text performSelectorOnMainThread:@selector(appendAttributedString:)
            withObject:response
            waitUntilDone:waitp];
    }

  [MLKDynamicContext popContext];
  LDESTROY (newctx);
  [statusText performSelectorOnMainThread:@selector(setStringValue:)
              withObject:@"Ready."
              waitUntilDone:waitp];

  [[text mutableString]
    performSelectorOnMainThread:@selector(appendString:)
    withObject:@"\n"
    waitUntilDone:waitp];

  [text performSelectorOnMainThread:@selector(endEditing)
        withObject:nil
        waitUntilDone:waitp];

  [self performSelectorOnMainThread:@selector(enableSubmitButton:)
        withObject:self
        waitUntilDone:NO];

  if (originalScrollPosition == 1.0)
    {
      [self performSelectorOnMainThread:@selector(scrollDown:)
            withObject:self
            waitUntilDone:NO];
    }

  [pool release];
}

- (void)enableSubmitButton:(id)sender
{
  [submitButton setEnabled:YES];
}

- (void)writeChar:(unichar)ch
{
  [self writeString:[NSString stringWithFormat:@"%C", ch]];
}

- (void)writeString:(NSString *)string
{
  float originalScrollPosition = [[[outputTextView enclosingScrollView]
                                    verticalScroller]
                                   floatValue];

  NSDictionary *attrs = [NSDictionary dictionaryWithObjectsAndKeys:
                                        [NSColor brownColor],
                                        NSForegroundColorAttributeName,
                                        nil];
  NSAttributedString *output =
    LAUTORELEASE ([[NSAttributedString alloc] initWithString:string
                                                  attributes:attrs]);
  [[outputTextView textStorage]
    performSelectorOnMainThread:@selector(appendAttributedString:)
    withObject:output
    waitUntilDone:YES];

  if (originalScrollPosition == 1.0)
    {
      [self performSelectorOnMainThread:@selector(scrollDown:)
            withObject:self
            waitUntilDone:NO];
    }
}

- (void)scrollDown:(id)sender
{
  // FIXME: This is slow.  Investigate NSClipView#-scrollToPoint as a
  // possible alternative.
  NSRange range = NSMakeRange ([[outputTextView textStorage] length], 0);
  [outputTextView scrollRangeToVisible:range];
}
@end
