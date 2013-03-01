import urwid

txt = urwid.Text(u"hello world")
fill = urwid.Filler(txt, 'top')
loop = urwid.MainLoop(fill)
loop.run()