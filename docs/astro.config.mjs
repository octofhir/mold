// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

// Hosted on GitHub Pages at https://octofhir.github.io/banshee/
export default defineConfig({
  site: 'https://octofhir.github.io',
  base: '/banshee',
  integrations: [
    starlight({
      title: 'banshee',
      description:
        'A PostgreSQL parser, formatter, linter, and language server written in Rust.',
      social: [
        { icon: 'github', label: 'GitHub', href: 'https://github.com/octofhir/banshee' },
      ],
      editLink: {
        baseUrl: 'https://github.com/octofhir/banshee/edit/main/docs/',
      },
      sidebar: [
        {
          label: 'Start here',
          items: [
            { label: 'Introduction', slug: 'index' },
            { label: 'Install', slug: 'install' },
            { label: 'Quickstart', slug: 'quickstart' },
          ],
        },
        {
          label: 'Guides',
          items: [{ autogenerate: { directory: 'guides' } }],
        },
        {
          label: 'Reference',
          items: [
            { label: 'CLI', slug: 'reference/cli' },
            { label: 'Library (crates)', slug: 'reference/library' },
          ],
        },
        {
          label: 'Lint rules',
          items: [{ autogenerate: { directory: 'rules', collapsed: true } }],
        },
      ],
    }),
  ],
});
