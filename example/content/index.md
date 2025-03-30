+++
title = '${name}\'s blog'
title_override = true
description = 'the blog made by $name'
+++

# Wiru's Blogs

## Current Posts:

<%for file in fs.list('post').sort((a, b) -> metadata(a).last_edit - metadata(b).last_edit) do%>
- [<% metadata(file).title %>](<% link('post/$file') %>)
<%end%>
