# rest-subsite

This is a playground project to answer a question posed to me - is
there a better way to manage publically-facing documentation for an
API?

There are some things I like about this approach, and some things that I don't.

I like the fact that code and documentation is all in one place. It's
much easier to both update and maintain. Keeping things in sync should
be much easier to do, and it's trivial to have field-by-field
explanations of what each part of a record type means and what values
you should expect. I also like that Haddock will complain if you don't
have at least a summary comment for a type, function, or module. We
could also fail CI if we see a diff in the generated haddock docs that
wasn't included in the PR.

I wasn't able to find a way to nit if there was a field that wasn't
documented, but it's possible this exists, or that we could cobble
something together that has this effect.

I'm less happy with the process. The Template Haskell approach I
adopted to get the static site loaded into the binary produces a LOT
of warnings about unused bindings, which is unfortunate. I could
disable this, perhaps by only generating the bindings in one file, so
that devs have a heads-up if other files they edit have this issue,
but it's still irksome.

Another issue is that (at least as currently implemented), the
documentation files aren't shipped with the binary. So if you want to
run this with documentation enabled, you need to bundle the docs/
directory along with. That's easy enough to do in a Docker container,
but it adds a lot of fussiness to a deploy if you aren't using
containers.

Lastly, Hoogle's UI is.... servicable... but it's not going to win any
awards for a beautiful color scheme or welcoming layout. It's possible
to massage things further, either by changing the theme Haddock uses
to generate the documentation pages or by doing things like loading
the individual pages in frame nested in more structure giving the
reader/aspiring developer some more support. But, if you already have
something that has a very slick UI, this will feel like a substantive
downgrade. But, I'm not a frontend dev, it's possible more work could
be done to shore this up while keeping the "generated from source
code" conveniences.
