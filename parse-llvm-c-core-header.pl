#! /usr/bin/env perl

# Used to be a one-liner:
#
#  perl -0777pe ...
#

open (INPUT, "<$ARGV[0]") or die;
@input = <INPUT>;
close (INPUT);
$text = join ("", @input);

# Kill uninteresting code sections.
$text =~ s/#define.*//g;
$text =~ s/#include.*//g;
$text =~ s/#ifndef.*((.|[\r\n])*)#endif/\1/g;
$text =~ s/#if DEBUG(.|[\r\n])*?#endif//g;
$text =~ s/#ifdef __cplusplus(.|[\r\n])*?#endif//g;

# Kill comments.
$text =~ s{/\*(.|[\r\n])*?\*/}{}g;

# Convert type specifiers.
$text =~ s/(LLVM[^(\n\r]+?Ref)(?=[^(])/:pointer/g;
$text =~ s/(const )?char \*/:string /g;
$text =~ s/(\w|-|:)+ \*/:pointer /g;
$text =~ s/unsigned long long/:unsigned-long-long/g;
$text =~ s/(<!-)((void)|(unsigned)|(long)|(int)) /:\1 /g;

# Kill argument names.
#$text =~ s/([(,][^ ]+) .*?(?=[,)])/\1/g;

# Convert enums into something useful.
@statements = split /;/, $text;  #/
@enum_names = ();
@new_statements = ();

foreach $statement (@statements)
  {
    $statement =~ s/^[ \t\n\r]*//;
    $statement =~ s/[ \t\n\r]*$//;
    $statement =~ s/[ \t\n\r]+/ /g;
    if ($statement =~ /^typedef enum {(.*?)} (.*)/
        or $statement =~ /^enum {(.*?)}.*$/)
      {
        $values = $1;
        @values = split /,/, $values;  #/;

        $name = $2;
        unless ($name =~ /^$/)
          {
            push @enum_names, ($name);
          }

        $value_value = -1;
        foreach $value (@values)
          {
            $value =~ s/^ //;
            $value =~ s/ $//;
            if ($value =~ /(.*?) ?= ?(.+)/)
              {
                $value_name = $1;
                $value_value = eval $2;  # for stuff like `1 << 4'
              }
            else
              {
                $value_name = $value;
                $value_value = $value_value + 1;
              }
            push @new_statements, ("(setq +$value_name+ $value_value)");
          }
      }
    elsif ($statement =~ /^typedef/)
      {
        # Ignore.
      }
    else
      {
        unless ($statement =~ /^ ?$/)
          {
            $statement =~ /^((?:\w|:|-)+) (\w+) ?\((.*)\)$/ or die "Don't understand: $statement";
            $return_type = $1;
            $c_name = $2;
            $arguments = $3;

            @argtypes = ();
            foreach (split /,/, $arguments)   #/
              {
                unless (/^void$/)
                  {
                    /^ ?((?:\w|:|-)+)(?: (.*))?$/ or die "Don't understand: $_ (in: $statement)";
                    push @argtypes, ($1);
                  }
              }

            $argtypes = join (" ", @argtypes);

            @lisp_name_components = ();
            $_ = $c_name;
            s/^LLVM//;
            while (/^(.*?)([A-Z])(.*)$/)
              {
                $start = $1;
                $lower = lc $2;
                $rest = $3;
                unless ($start =~ /^$/)
                  {
                    $start =~ /^\w+$/ or die "Weird stuff here: $start";
                    push @lisp_name_components, ($start);
                  }
                $_ = "${lower}${rest}";
              }

            push @lisp_name_components, ($_);
            $lisp_name = join ("-", @lisp_name_components);

            push @new_statements, "(define-foreign-function $lisp_name \"$c_name\" nil $return_type $argtypes)";
          }
      }
  }

$text = join ("\n", @new_statements);

# Replace enum types with :int.
foreach $type (@enum_names)
  {
    $text =~ s/$type /:int /g;
  }

if ($ARGV[1] =~ /^$/)
  {
    print "$text\n";
  }
else
  {
    open (OUTPUT, ">$ARGV[1]") or die "Couldn't open file for writing: $ARGV[1]";
    print (OUTPUT "$text\n");
    close (OUTPUT);
  }
