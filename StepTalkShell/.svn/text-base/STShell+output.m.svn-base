/**
    STShell+output
 
    Copyright (c) 2002 Free Software Foundation
 
    Written by: Stefan Urbanek <urbanek@host.sk>
    Date: 2002 Jun 7
   
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

#import "STShell.h"

#import <StepTalk/StepTalk.h>

#import <Foundation/NSArray.h>
#import <Foundation/NSBundle.h>
#import <Foundation/NSDebug.h>
#import <Foundation/NSNotification.h>
#import <Foundation/NSException.h>
#import <Foundation/NSSet.h>
#import <Foundation/NSString.h>
#import <Foundation/NSValue.h>

#include <readline/readline.h>

@implementation STShell(STShellOutput)
- show:(id)anObject
{
    printf("%s", [[anObject description] cString]);

    return self;
}
- showLine:(id)anObject
{
    [self show:anObject];
    putchar('\n');
    
    return nil;
}
- (void)showError:(NSString *)errString
{
    fprintf(stderr, "%s\n\n", [errString cString]);
}

- showResult:(id)obj
{
    const char *className = [NSStringFromClass([obj class]) cString];
    int         objIndex = [objectStack count] - 1;
    int         i;
    
    if(obj)
    {
        if([obj isKindOfClass:[NSArray class]])
        {
            printf("(%i) %s\n", objIndex, className);
            
            for(i = 0;i<[obj count]; i++)
            {
                printf("%i  %s\n", i, 
                       [self displayCStringForObject:[obj objectAtIndex:i]]); 
            }
            
        }
        else if([obj isKindOfClass:[NSSet class]])
        {
            printf("(%i) %s\n", objIndex, className);
            
            obj = [[obj allObjects] sortedArrayUsingSelector:@selector(compare:)];
            for(i = 0;i<[obj count]; i++)
            {
                printf("%s\n", 
                       [self displayCStringForObject:[obj objectAtIndex:i]]); 
            }
            
        }
        else if([obj isKindOfClass:[NSDictionary class]])
        {
            NSString *key;
            NSArray  *keys;
            
            printf("(%i) %s\n", objIndex, className);
            
            keys = [[obj allKeys] sortedArrayUsingSelector:@selector(compare:)];

            for(i = 0;i<[keys count]; i++)
            {
                key = [keys objectAtIndex:i];
                printf("%s : %s\n",  
                       [self displayCStringForObject:key], 
                       [self displayCStringForObject:[obj objectForKey:key]]); 
            }   
        }
        else
        {
            printf("(%i) %s\n", objIndex, [self displayCStringForObject:obj]);
        }
    }

    return self;
}
- (char *)displayCStringForObject:(id)object
{
    NSString *str = [object description];
        
    if( [str length] > 60 )
    {
        str = [str substringToIndex:60];
        str = [str stringByAppendingString:@"..."];
    }
    
    return [str cString];
}

- showException:(NSException *)exception
{
    printf("Error (%s): %s\n", 
            [[exception name] cString], 
            [[exception reason] cString]);

    
    return self;
}

- (id)listObjects
{
    NSString *str;
    int       i;
    id        object;
    
    printf("Objects\n");
    for(i = 0; i < [objectStack count]; i++)
    {
        object = [objectStack objectAtIndex:i];
        
        str = [object description];
        
        if( [str length] > 60 )
        {
            str = [str substringToIndex:60];
            str = [str stringByAppendingString:@"..."];
        }
        
        printf("%4i: '%s' (%s)\n", i,
                [str cString],
                [[[object class] description] cString]);
    }
    return nil;
}
@end
