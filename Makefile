srfi-123.html: srfi-123.md
	pandoc \
	  --from=markdown_github-hard_line_breaks+pandoc_title_block \
	  --standalone \
	  --to=html \
	  --css=http://srfi.schemers.org/srfi.css \
	  srfi-123.md \
	  >srfi-123.html
