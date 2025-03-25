#!/usr/bin/perl
use strict;
use warnings;
use feature 'say';

my %variables;

while (1) {
    print "Enter an expression (or 'q' to quit): ";
    my $input = <STDIN>;
    chomp $input;
    
    last if $input eq 'q';
    next if $input =~ /^\s*$/;
    
    unless ($input =~ /^[a-zA-Z0-9_+\-*\/=()\s]+$/) {
        say "Error: Invalid characters in input";
        next;
    }
    
    if ($input =~ /=/) {
        my ($var, $expr) = split /\s*=\s*/, $input, 2;
        
        unless ($var =~ /^[a-zA-Z][a-zA-Z0-9_]*$/) {
            say "Error: Invalid variable name";
            next;
        }
        
        my ($result, $error) = evaluate_expression($expr);
        if (defined $error) {
            say "Error: $error";
        } else {
            $variables{$var} = $result;
            say "Assigned: $var = $result";
        }
        next;
    }
    
    my ($result, $error) = evaluate_expression($input);
    if (defined $error) {
        say "Error: $error";
    } else {
        say "Result: $result";
    }
}

sub evaluate_expression {
    my ($expr) = @_;
    $expr =~ s/\s+//g;
    
    # Paranthesis control
    my $paren_count = 0;
    for my $c (split //, $expr) {
        $paren_count++ if $c eq '(';
        $paren_count-- if $c eq ')';
        return (undef, "Mismatched parentheses") if $paren_count < 0;
    }
    return (undef, "Mismatched parentheses") if $paren_count != 0;
    
    # Match variables with values
    eval {
        $expr =~ s/([a-zA-Z][a-zA-Z0-9_]*)/exists $variables{$1} ? $variables{$1} : die "Undefined variable: $1"/ge;
    };
    if ($@) {
        return (undef, $@ =~ s/ at .*? line \d+.*//r);
    }
    
    # Invalid expression check
    unless ($expr =~ /^[0-9+\-*\/()]+$/) {
        return (undef, "Invalid expression");
    }
    
    # Calculation and divison by zero
    my $result;
    eval {
        # For division by zero
        if ($expr =~ m{(?<!\d)/(?!\d)|\b0\s*/|\/\s*0\b}) {
            die "Division by zero";
        }
        $result = eval $expr;
        die $@ if $@;
    };
    
    if ($@) {
        my $error = $@;
        $error =~ s/ at .*? line \d+.*//;
        return (undef, $error);
    }
    
    return ($result, undef);
}