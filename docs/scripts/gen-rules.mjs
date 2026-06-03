// Generates one Starlight page per lint rule from the banshee binary, so the
// rule reference can never drift from the implementation. Run via `npm run
// gen:rules` (also wired into `npm run build`).
//
// Reads `banshee rules --format json` for the catalog and `banshee explain
// <CODE>` for each rule's prose, then writes src/content/docs/rules/<code>.md.
import { execFileSync } from 'node:child_process';
import { mkdirSync, writeFileSync, rmSync, readdirSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const repo = join(here, '..', '..');
const BANSHEE = process.env.BANSHEE || join(repo, 'target', 'release', 'banshee');
const OUT = join(here, '..', 'src', 'content', 'docs', 'rules');

const GROUPS = {
  AL: 'Aliasing', AM: 'Ambiguity', ST: 'Structure', SF: 'Safety', JB: 'JSONB',
  CV: 'Convention', CP: 'Capitalisation', RF: 'References', MG: 'Migration safety',
};

const run = (...args) => execFileSync(BANSHEE, args, { encoding: 'utf8' });
const esc = (s) => s.replace(/"/g, '\\"');

function parseExplain(text) {
  // First line: "CODE: summary (auto-fixable)". Then prose, then "bad:/good:".
  const lines = text.replace(/\r/g, '').split('\n');
  const head = lines.shift() ?? '';
  const fixable = /\(auto-fixable\)\s*$/.test(head);
  const body = lines.join('\n').trim();
  // Split prose from the example block (indented bad:/good: lines).
  const exIdx = body.search(/^\s+(bad|good):/m);
  let prose = body, example = '';
  if (exIdx >= 0) {
    prose = body.slice(0, exIdx).trim();
    example = body
      .slice(exIdx)
      .split('\n')
      .map((l) => l.replace(/^\s{2}/, ''))
      .join('\n')
      .trim();
  }
  return { fixable, prose, example };
}

const catalog = JSON.parse(run('rules', '--format', 'json'));
catalog.sort((a, b) => a.code.localeCompare(b.code));

rmSync(OUT, { recursive: true, force: true });
mkdirSync(OUT, { recursive: true });

for (const { code, group, fixable, summary } of catalog) {
  const explain = parseExplain(run('explain', code));
  const prefix = code.slice(0, 2);
  const groupName = GROUPS[prefix] ?? group ?? 'Other';
  const fix = fixable || explain.fixable;

  const fm = [
    '---',
    `title: "${esc(code)} — ${esc(summary)}"`,
    `description: "${esc(summary)}"`,
    'tableOfContents: false',
    '---',
    '',
  ].join('\n');

  const badges =
    `**Group:** ${groupName} · **Auto-fixable:** ${fix ? 'yes' : 'no'}\n`;

  const prose = explain.prose ? `\n${explain.prose}\n` : '';
  const example = explain.example
    ? `\n## Example\n\n\`\`\`sql\n${explain.example}\n\`\`\`\n`
    : '';
  const cfg =
    `\n## Configure\n\nDisable this rule in \`banshee.toml\`:\n\n` +
    `\`\`\`toml\n[lint]\nexclude = ["${code}"]\n\`\`\`\n\n` +
    `Or silence one line with \`-- noqa: ${code}\`. See ` +
    `[Linting](/banshee/guides/linting/).\n`;

  writeFileSync(join(OUT, `${code}.md`), fm + badges + prose + example + cfg);
}

// Build the index table grouped by prefix.
const byGroup = new Map();
for (const r of catalog) {
  const p = r.code.slice(0, 2);
  if (!byGroup.has(p)) byGroup.set(p, []);
  byGroup.get(p).push(r);
}
let idx =
  '---\ntitle: Lint rules\ndescription: Every banshee lint rule, with rationale and examples.\n---\n\n' +
  `banshee ships ${catalog.length} lint rules. Each has a dedicated page with the ` +
  'rationale, a before/after example, and how to configure or suppress it. ' +
  'Run `banshee explain <CODE>` for the same text on the command line.\n\n';
for (const [p, rules] of [...byGroup.entries()].sort()) {
  idx += `## ${GROUPS[p] ?? p}\n\n| Code | Fixable | Description |\n|------|---------|-------------|\n`;
  for (const r of rules.sort((a, b) => a.code.localeCompare(b.code))) {
    idx += `| [${r.code}](/banshee/rules/${r.code.toLowerCase()}/) | ${r.fixable ? 'yes' : '—'} | ${r.summary} |\n`;
  }
  idx += '\n';
}
writeFileSync(join(OUT, 'index.md'), idx);

console.log(`generated ${catalog.length} rule pages + index in ${OUT}`);
