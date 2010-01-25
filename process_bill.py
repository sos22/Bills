#! /usr/bin/env python

import sys
import os
import popen2
import optparse
import tempfile
import re
import cPickle
import urllib
import urllib2

class Database:
    def __init__(self):
        self.people = []
        self.item_lookup = {}
        
class Item:
    def __init__(self, name, quantity, price, fraction = 1):
        self.name = name
        self.quantity = quantity
        self.price = price
        self.fraction = fraction
    def charge(self):
        return self.price / self.fraction
    def fractional(self, fract):
        return Item(self.name, self.quantity, self.price, fract)
    def __str__(self):
        if self.fraction > 1.001 or self.fraction < 0.999:
            return "%-50s * %3s / %1.0f = %.2f" % (self.name, self.quantity,
                                                   self.fraction,
                                                   self.charge())
        else:
            return "%-50s * %7s = %.2f" % (self.name, self.quantity,
                                           self.charge())

def scan_and_ocr():
    (fd, scanned_image) = tempfile.mkstemp(suffix=".tif", prefix="process_bill")
    os.close(fd)
    (stdout, stdin, stderr) = \
             popen2.popen3("scanimage --format=tiff -p > %s" % scanned_image)
    b = ""
    while True:
        l = stderr.read(5)
        if l == "":
            break
        for c in l:
            b += c
            if c == "\r":
                print ("Scanning %s" % b),
                b = ""

    stdout.close()
    stdin.close()
    stderr.close()
    print

    (fd, ocr_output) = tempfile.mkstemp(prefix="process_bill")
    os.close(fd)
    os.unlink(ocr_output)

    print "OCR output in %s" % ocr_output
    r = os.system("tesseract %s %s" % (scanned_image, ocr_output))
    if r != 0:
        print "tesseract failed with code %d" % r
        sys.exit(1)
    os.unlink(scanned_image)

    f = file("%s.txt" % ocr_output)
    result = f.read()
    f.close()
    print "ocr results in %s" % ocr_output
    #os.unlink("%s.txt" % ocr_output)

    return result

def contains(haystack, needle):
    if len(haystack) < len(needle):
        return False
    for x in xrange(0, len(haystack) - len(needle) + 1):
        if haystack[x:x+len(needle)] == needle:
            return True
    return False

class Order:
    def __init__(self, itms = None):
        if itms:
            self.items = itms
        else:
            self.items = []
    def total(self):
        return sum([x.charge() for x in self.items])
    def _extract_interesting_lines(self, lines):
        res = []
        in_header = True
        for l in lines:
            w = l.split()
            if in_header:
                if contains(w, ["Quantity", "Unit", "Price", "Total" ]):
                    in_header = False
            else:
                if len(w) == 1 and w[0] in ["Freezer", "Fridge", "Cupboard"]:
                    # Don't want section headings
                    continue
                if len(w) == 2 and w == ["Original", "copy"]:
                    # End of order
                    break
                if len(w) >= 6 and w[0] == "Items" and w[1] == "marked" and w[2] == "with" and w[4] == "include" and w[5] == "VAT":
                    # End of order
                    break
                if len(w) >= 3 and w[:3] == ["Payments", "and" ,"savings"]:
                    break
                if len(w) >= 4 and w[:4] == ["Cost", "of", "products",
                                             "pre-promotions"]:
                    break
                if len(w) < 4:
                    # Very short lines are usually OCR errors.
                    continue
                res.append(w)
        return res
    def add_page(self):
        text = scan_and_ocr()
        lines = text.split("\n")
        lines = self._extract_interesting_lines(lines)
        def _parse_item(w):
            total = float(w[-1])
            unit_price = float(w[-2])
            if w[-3] == "Kg":
                quantity = w[-4] + " " + w[-3]
                quantity_n = float(w[-4])
                name = w[:-4]
            else:
                quantity = w[-3]
                quantity_n = float(w[-3])
                name = w[:-3]
            if name[-1] == "*":
                name = name[:-1]
            name = " ".join(name)
            exp_total = quantity_n * unit_price
            if exp_total - total <= -0.01 or exp_total - total >= 0.01:
                print "line %s doesn't add up (total %f, unit_price %f, quantity %f)" % (w,
                                                                                         total,
                                                                                         unit_price,
                                                                                         quantity)
                sys.exit(1)
            return Item(name, quantity, total)
        def parse_item(w):
            try:
                return _parse_item(w)
            except Exception, e:
                try:
                    return _parse_item(w[:-1])
                except:
                    print "except %s parsing %s" % (str(e), w)
                    return Item("<bad %s>" % (" ".join(w)), 1, 1)
        new_items = map(parse_item, lines)
        self.items += new_items

    def __str__(self):
        return "%s\n\nTotal:\t\t%f" % ("\n".join(map(str, self.items)), self.total())
    
parser = optparse.OptionParser()

delete_scanned_image = False
delete_ocr_output = False

parser.add_option("-d", "--database", help="use this item->person database",
                  dest="database", metavar="FILE",
                  default="/home/sos22/etc/tesco_bill.db")
parser.add_option("-u", "--user", help="username to log in to the bills server",
                  dest="uname", metavar="USERNAME")
parser.add_option("-p", "--passwd", help="passwd to log in to the bills server",
                  dest="passwd", metavar="PASSWORD")
parser.add_option("-s", "--server", help="hostname of bills server",
                  dest="server", metavar="SERVER")

(options,args) = parser.parse_args()

if args != []:
    print "didn't expect any positional arguments"
    sys.exit(1)
if options.uname == None:
    print "Need a username"
    sys.exit(1)
if options.passwd == None:
    print "Need a password"
    sys.exit(1)
if options.server == None:
    print "Need a server"
    sys.exit(1)
    
try:
    f = file(options.database)
    database = cPickle.load(f)
    f.close()
except:
    database = Database()

order = Order()

def yes_no_choice(prompt):
    while True:
        print prompt
        r = sys.stdin.readline().strip()
        if r == "y":
            return True
        if r == "n":
            return False
        print "Expected either y or n"
        
while True:
    order.add_page()
    more_pages = yes_no_choice("Another page?")
    if not more_pages:
        break

print "Date?"
date = sys.stdin.readline().strip()

while True:
    print "Delivery?"
    r = sys.stdin.readline().strip()
    try:
        delivery = float(r)
        break
    except:
        print "Expected a number"

print "Final order:\n%s" % str(order)

def lookup_person(name):
    def is_prefix(needle, haystack):
        return len(needle) <= len(haystack) and needle == haystack[:len(needle)]
    for candidate in database.people:
        if is_prefix(name, candidate):
            return candidate
    add_it = yes_no_choice("%s unknown.  Add to database?" % name)
    if not add_it:
        return None
    while True:
        print "Actual name of person?"
        actual = sys.stdin.readline().strip().lower()
        if actual == "":
            return None
        if is_prefix(name, actual):
            break
        print "Expected %s to be a prefix of the actual name" % name
    database.people.append(actual)
    return actual

def edit_charge_item(i):
    print "Actual name of item?"
    name = sys.stdin.readline().strip()
    print "Actual quantity?"
    quantity = float(sys.stdin.readline().strip())
    print "Actual price?"
    price = float(sys.stdin.readline().strip())
    p = "Change %f %s at %s to %f %s at %s?" % (i.quantity, i.name, i.price,
                                                quantity, name, price)
    if yes_no_choice(p):
        i.name = name
        i.quantity = quantity
        i.price = price
    
def assign_item_to_person(i):
    if database.item_lookup.has_key(i.name.lower()):
        i.chargees = database.item_lookup[i.name.lower()]
        return
    while True:
        print "%s" % i
        l = sys.stdin.readline().strip().lower()
        if l == "":
            continue
        if l == "!":
            try:
                edit_charge_item(i);
            except:
                pass
            continue
        if l[0] == '*':
            add_db_entry = True
            l = l[1:]
        else:
            add_db_entry = False
        people = map(lookup_person, l.split("+"))
        if None in people:
            continue
        break
    if add_db_entry:
        database.item_lookup[i.name.lower()] = people
    i.chargees = people
    
for i in order.items:
    assign_item_to_person(i)

print order

person_orders = {}
for i in order.items:
    n_chargees = len(i.chargees)
    if n_chargees == 0:
        print "%s has no chargee!" % i
    for c in i.chargees:
        if not person_orders.has_key(c):
            person_orders[c] = Order()
        person_orders[c].items.append(i.fractional(n_chargees))

tot_charges = 0
for i in person_orders.itervalues():
    tot_charges += i.total()

for i in person_orders.itervalues():
    i.items.append(Item("delivery", 1, (delivery*i.total())/tot_charges, 1))
    
for person in person_orders.iterkeys():
    print "%s:\n" % person.capitalize()
    print str(person_orders[person])
    print "\n\n-------------------------------------------------\n\n"

f = file(options.database, "w")
cPickle.dump(database, f)
f.close()

cmd = "addbill.py -u %s -p %s -s %s -d \"Tesco order\" -D %s " % (options.uname,
                                                                  options.passwd,
                                                                  options.server,
                                                                  date)

for (person, order) in person_orders.items():
    cmd += "-P %s=%f " % (person.capitalize(), order.total())
cmd += "-r %s=%f " % (options.uname.capitalize(), tot_charges + delivery)

(fd, l) = tempfile.mkstemp()
os.close(fd)
f = file(l, "w")

for person in person_orders.iterkeys():
    f.write(person.capitalize())
    f.write("\n\n")
    f.write(str(person_orders[person]))
    f.write("\n\n---------------------------------------------------\n\n")

f.close()

cmd += "-a %s" % l

print cmd

os.system(cmd)
