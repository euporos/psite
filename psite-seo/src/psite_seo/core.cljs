(ns psite-seo.core
  "Convenience re-exports of the most-used psite-seo fns. Import the
  submodules directly for the full surface:
  - psite-seo.meta    — <meta> + <title>
  - psite-seo.link    — <link>
  - psite-seo.json-ld — schema.org JSON-LD"
  (:require [psite-seo.json-ld :as ld]
            [psite-seo.link    :as link]
            [psite-seo.meta    :as m]))

(def title                     m/title)
(def description               m/description)
(def keywords                  m/keywords)
(def robots                    m/robots)
(def viewport                  m/viewport)
(def charset                   m/charset)
(def http-equiv                m/http-equiv)
(def google-site-verification  m/google-site-verification)
(def open-graph                m/open-graph)
(def twitter-card              m/twitter-card)

(def canonical                 link/canonical)
(def alternate                 link/alternate)
(def alternates                link/alternates)
(def favicon-set               link/favicon-set)

(def ld                        ld/ld)
(def entity                    ld/entity)
(def breadcrumb-list           ld/breadcrumb-list)
(def music-event               ld/music-event)
(def event                     ld/event)
(def organization              ld/organization)
(def person                    ld/person)
(def website                   ld/website)
(def article                   ld/article)
(def item-list                 ld/item-list)
