import remarkParse from 'remark-parse'
import {unified} from 'unified'

const processor = unified().use(remarkParse)

const value = `
# Pluto

**Pluto** (minor-planet designation: *134340 Pluto*) is a [dwarf planet](https://en.wikipedia.org/wiki/Dwarf_planet)
in the [Kuiper belt](https://en.wikipedia.org/wiki/Kuiper_belt).
`

const ast = processor.parse(value)

console.log(JSON.stringify(ast, null, 2))
