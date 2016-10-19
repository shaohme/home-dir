#!/usr/bin/perl
# License: Same as GTK2
use strict;
use warnings;
use Gtk2 '-init';

my $event = Gtk2::Gdk::Event->new("GDK_CLIENT_EVENT");
$event->send_event(1);
$event->window(undef);
$event->message_type(Gtk2::Gdk::Atom->intern("_GTK_READ_RCFILES", 0));
$event->data_format(8);
Gtk2::Gdk::Event->send_clientmessage_toall($event);
