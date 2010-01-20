#! /usr/bin/env python

import simplejson as json
import httplib
import sys
import optparse
import urllib

parser = optparse.OptionParser()
parser.add_option("-u", "--user", dest="username",
                  help="Username to log in with", metavar="USERNAME")
parser.add_option("-p", "--passwd", dest="password",
                  help="Password for login", metavar="PASSWORD")
parser.add_option("-s", "--server", dest="server",
                  help="Server host name", metavar="SERVER",
                  default="localhost")
parser.add_option("-d", "--description", dest="description",
                  help="Description of bill", metavar="DESCRIPTION")
parser.add_option("-D", "--date", dest="date",
                  help="Date of bill", metavar="YYYY-MM-DD")
parser.add_option("-P", "--pay", dest="payers",
                  help="Who pays", metavar="UNAME=AMOUNT",
                  action="append")
parser.add_option("-r", "--receive", dest="receivers",
                  help="Who gets reimbursed", metavar="UNAME=AMOUNT",
                  action="append")
parser.add_option("-a", "--attach", dest="attachments",
                  action="append", help="List of files to attach",
                  metavar="PATH")

(opts, args) = parser.parse_args()

if args != []:
    raise "extra arguments %s" % str(args)

payers = []
if opts.payers:
    for p in opts.payers:
        [u, a] = p.split("=")
        payers.append((u, a))
recvs = []
if opts.receivers:
    for p in opts.receivers:
        [u, a] = p.split("=")
        recvs.append((u, a))

def do_request(server, action, args):
    h = httplib.HTTPConnection(server, 8000)
    u = urllib.urlencode(args)
    u = "http://%s:8000/action/%s?%s" % (server, action, u)
    h.request("GET", u)
    response = h.getresponse().read()
    r = json.loads(response)
    if r["result"] == "okay":
        if r.has_key("data"):
            return r["data"]
        else:
            return True
    elif r["result"] == "error":
        raise r["error"]
    else:
        raise "Bad response from server: %s" % response
    
def do_login(uname, password, server):
    return do_request(server, "login_raw", {"uname":uname,
                                            "password":password})

r = do_login(opts.username, opts.password, opts.server)
cookie = r["cookie"]

def do_add_bill(server, cookie, description, date, recvs, payers):
    def encode_list(items):
        r = []
        for (uname, amount) in items:
            r.append({"user": uname, "charge": amount})
        return json.dumps(r)
    recvs = encode_list(recvs)
    payers = encode_list(payers)
    r = do_request(server, "add_bill", {"cookie": cookie,
                                        "description": description,
                                        "date": date,
                                        "to_pay": payers,
                                        "to_receive": recvs})
    return r
    
bill_id = do_add_bill(opts.server, cookie, opts.description, opts.date, recvs, payers)

# Python's standard library, rather shockingly, doesn't include any
# way of encoding multipart form data, so use the ones from
# http://code.activestate.com/recipes/146306/

def post_multipart(host, selector, fields, files):
    """
    Post fields and files to an http host as multipart/form-data.
    fields is a sequence of (name, value) elements for regular form fields.
    files is a sequence of (name, filename, value) elements for data to be uploaded as files
    Return the server's response page.
    """
    content_type, body = encode_multipart_formdata(fields, files)
    h = httplib.HTTP(host, 8000)
    h.putrequest('POST', selector)
    h.putheader('content-type', content_type)
    h.putheader('content-length', str(len(body)))
    h.putheader("referer", "nobody at all")
    h.endheaders()
    h.send(body)
    return h.getreply()

def encode_multipart_formdata(fields, files):
    """
    fields is a sequence of (name, value) elements for regular form fields.
    files is a sequence of (name, filename, value) elements for data to be uploaded as files
    Return (content_type, body) ready for httplib.HTTP instance
    """
    BOUNDARY = '----------ThIs_Is_tHe_bouNdaRY_$'
    CRLF = '\r\n'
    L = []
    for (key, value) in fields:
        L.append('--' + BOUNDARY)
        L.append('Content-Disposition: form-data; name="%s"' % key)
        L.append('')
        L.append(value)
    for (key, filename, value) in files:
        L.append('--' + BOUNDARY)
        L.append('Content-Disposition: form-data; name="%s"; filename="%s"' % (key, filename))
        L.append('Content-Type: %s' % get_content_type(filename))
        L.append('')
        L.append(value)
    L.append('--' + BOUNDARY + '--')
    L.append('')
    body = CRLF.join(L)
    content_type = 'multipart/form-data; boundary=%s' % BOUNDARY
    return content_type, body

def get_content_type(filename):
    return 'application/octet-stream'

def do_attach_file(server, cookie, path, bill):
    response = post_multipart(server,
                              "http://%s:8000/forms/attach_file" % server,
                              [("cookie", cookie),
                               ("bill", str(bill))],
                              [("file_to_attach",
                                path,
                                file(path).read())])
    return int(response[2].getheader("X-AttachmentId"))
    
    
if opts.attachments:
    for a in opts.attachments:
        aid = do_attach_file(opts.server, cookie, a, bill_id)
        print aid
        
