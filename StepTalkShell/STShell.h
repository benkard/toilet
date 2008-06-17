/**
    STShell
    StepTalk Shell
 
    Copyright (c) 2002 Free Software Foundation
 
    Written by: Stefan Urbanek <urbanek@host.sk>
    Date: 2002 May 29
   
    This file is part of the StepTalk project.
 
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
 
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
 
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02111, USA.
 
 */

#import <Foundation/NSObject.h>

@class STConversation;
@class STEnvironment;
@class STScriptsManager;
@class NSMutableArray;
@class NSException;

@interface STShell:NSObject
{
    STScriptsManager *scriptsManager;
    STConversation   *conversation;
    
    NSString         *prompt;
    NSString         *source;
    
    NSMutableArray   *objectStack;
    
    BOOL              exitRequest;

    BOOL              updateCompletionList;
    NSArray          *completionList;
    BOOL              completionEnabled;

}
- initWithEnvironment:(STEnvironment *)env;

- (void)setLanguage:(NSString *)langName;
- (void)setEnvironment:(STEnvironment *)newEnv;
- (STEnvironment *)environment;

- (void)run;

- show:(id)anObject;
- showLine:(id)anObject;

- (id)executeLine:(NSString *)line;

- showResult:(id)obj;
- showException:(NSException *)exception;
- (void)showError:(NSString *)errString;
@end

@interface STShell(STShellOutput)

- show:(id)anObject;
- showLine:(id)anObject;
- showResult:(id)obj;
- (char *)displayCStringForObject:(id)object;
- showException:(NSException *)exception;
- (id)listObjects;

@end
