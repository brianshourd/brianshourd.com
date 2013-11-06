---
title: Simple Doc Builder
author: Brian Shourd
date: November 6, 2013
tags: coding, javascript
---

Yesterday I built a fun script to turn simple markdown documents into
simple webpages. It's just a simple (and fragile) parser for breaking a
markdown document into a title, sections, and subsections, packaging
that into a JS object (with help from [markdown-js]), and then running
it through [handlebars.js]. The object basically looks like:

~~~{.javascript}
{
    title: String,
    sections: [{
        title: String,
        id: String,
        body: String (HTML),
        subsections: [{
            title: String,
            subtitle: String,
            id: String,
            body: String (HTML)
        }]
    }]
}
~~~

This gives you enough information to, for example, construct a
handy-dandy table of contents (a-la [pandoc], but with a restricted set of
inputs and a much more configurable output). Here's the default
table of contents template:

~~~{.html}
{{#each sections}}
<a class="toc_title" href="#{{id}}">
    {{title}}
</a>
{{#if subsections}}
<ul class="toc_section">
    {{#each subsections}}
    <li>- <a href="#{{id}}">{{title}}</a> -</li>
    {{/each}}
</ul>
{{/if}}
{{/each}}
~~~

The resultant code is called [simple-doc-builder]. Here's [an
example][readme-ex] of what it can do. It isn't groundbreaking by any
means, but it was a lot of fun to make, and it makes my simple writings
look nice with no effort.

[handlebars.js]: http://handlebarsjs.com
[markdown-js]: https://github.com/evilstreak/markdown-js
[simple-doc-builder]: https://github.com/brianshourd/simple-doc-builder
[readme-ex]: http://brianshourd.github.io/simple-doc-builder/readme.html
[pandoc]: http://johnmacfarlane.net/pandoc/
