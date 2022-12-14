// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

const organizationName = 'dzfrias';
const projectName = 'scurry';

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Scurry',
  tagline: 'An New Take on Object-Oriented Languages',
  url: `https://${organizationName}.github.io`,
  trailingSlash: false,
  baseUrl: '`/${projectName}/`',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: '/img/favicon.ico',

  organizationName,
  projectName,

  // Even if you don't use internalization, you can use this field to set useful
  // metadata like html lang. For example, if your site is Chinese, you may want
  // to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
        },
        blog: {
          showReadingTime: true,
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        title: 'Scurry',
        logo: {
          alt: 'Scurry logo',
          src: 'img/logo.svg',
        },
        items: [
          {
            type: 'doc',
            docId: 'intro',
            position: 'left',
            label: 'Documentation',
          },
          {to: '/blog', label: 'Blog', position: 'left'},
          {
            href: 'https://crates.io/crates/scurry',
            label: 'crates.io',
            position: 'right',
          },
          {
            href: 'https://github.com/dzfrias/scurry',
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Docs',
            items: [
              {
                label: 'Full Documentation',
                to: '/docs/intro',
              },
            ],
          },
          {
            title: 'More',
            items: [
              {
                label: 'Website',
                href: 'https://dzfrias.github.io/scurry-web/',
              },
              {
                label: 'GitHub',
                href: 'https://github.com/dzfrias/scurry',
              },
              {
                label: 'crates.io',
                href: 'https://crates.io/crates/scurry',
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} Diego Frias`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
      },
    }),
};

module.exports = config;
