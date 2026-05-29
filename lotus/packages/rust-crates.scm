;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2026 Daniel Khodabakhsh <d@niel.khodabakh.sh>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (lotus packages rust-crates)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module ((gnu packages rust-sources) #:prefix package:)
  #:export (lookup-cargo-inputs))



(let ((src (resolve-module '(gnu packages rust-crates)))
      (dst (current-module)))
  (module-for-each
   (lambda (sym var)
     (module-define! dst sym (variable-ref var))
     (module-export! dst (list sym)))
   src))


(define-cargo-inputs lookup-cargo-inputs
  (rust-usrhttpd-v0.1.0 => (list rust-adler2-2.0.1
                                 rust-aho-corasick-1.1.4
                                 rust-android-system-properties-0.1.5
                                 rust-anstream-0.6.21
                                 rust-anstyle-1.0.13
                                 rust-anstyle-parse-0.2.7
                                 rust-anstyle-query-1.1.5
                                 rust-anstyle-wincon-3.0.11
                                 rust-anyhow-1.0.102
                                 rust-atomic-waker-1.1.2
                                 rust-autocfg-1.5.0
                                 rust-base64-0.22.1
                                 rust-bitflags-2.11.0
                                 rust-block-buffer-0.10.4
                                 rust-bumpalo-3.20.2
                                 rust-bytes-1.11.1
                                 rust-cc-1.2.56
                                 rust-cfg-if-1.0.4
                                 rust-chrono-0.4.43
                                 rust-clap-4.5.60
                                 rust-clap-builder-4.5.60
                                 rust-clap-derive-4.5.55
                                 rust-clap-lex-1.0.0
                                 rust-colorchoice-1.0.4
                                 rust-core-foundation-sys-0.8.7
                                 rust-cpufeatures-0.2.17
                                 rust-crc32fast-1.5.0
                                 rust-crypto-common-0.1.7
                                 rust-digest-0.10.7
                                 rust-dirs-6.0.0
                                 rust-dirs-sys-0.5.0
                                 rust-equivalent-1.0.2
                                 rust-errno-0.3.14
                                 rust-find-msvc-tools-0.1.9
                                 rust-flate2-1.1.9
                                 rust-fnv-1.0.7
                                 rust-form-urlencoded-1.2.2
                                 rust-futures-channel-0.3.32
                                 rust-futures-core-0.3.32
                                 rust-futures-sink-0.3.32
                                 rust-futures-task-0.3.32
                                 rust-futures-util-0.3.32
                                 rust-futures-macro-0.3.32
                                 rust-generic-array-0.14.7
                                 rust-getopts-0.2.24
                                 rust-getrandom-0.2.17
                                 rust-h2-0.4.13
                                 rust-hashbrown-0.16.1
                                 rust-heck-0.5.0
                                 rust-hex-0.4.3
                                 rust-http-1.4.0
                                 rust-http-body-1.0.1
                                 rust-http-body-util-0.1.3
                                 rust-httparse-1.10.1
                                 rust-httpdate-1.0.3
                                 rust-hyper-1.8.1
                                 rust-hyper-util-0.1.20
                                 rust-iana-time-zone-0.1.65
                                 rust-iana-time-zone-haiku-0.1.2
                                 rust-idna-1.1.0
                                 rust-idna-adapter-1.1.0
                                 rust-idna-mapping-1.0.0
                                 rust-indexmap-2.13.0
                                 rust-is-terminal-polyfill-1.70.2
                                 rust-itoa-1.0.17
                                 rust-js-sys-0.3.88
                                 rust-lazy-static-1.5.0
                                 rust-libc-0.2.182
                                 rust-libredox-0.1.9
                                 rust-lock-api-0.4.14
                                 rust-log-0.4.29
                                 rust-memchr-2.8.0
                                 rust-mime-0.3.17
                                 rust-mime-guess-2.0.5
                                 rust-miniz-oxide-0.8.9
                                 rust-mio-1.1.1
                                 rust-nu-ansi-term-0.50.3
                                 rust-num-traits-0.2.19
                                 rust-once-cell-1.21.3
                                 rust-once-cell-polyfill-1.70.2
                                 rust-option-ext-0.2.0
                                 rust-parking-lot-0.12.5
                                 rust-parking-lot-core-0.9.12
                                 rust-percent-encoding-2.3.2
                                 rust-pin-project-lite-0.2.16
                                 rust-pin-utils-0.1.0
                                 rust-proc-macro2-1.0.106
                                 rust-pulldown-cmark-0.9.6
                                 rust-quote-1.0.44
                                 rust-redox-syscall-0.5.18
                                 ;; rust-redox-users-0.5.2
                                 rust-redox-users-0.4.6
                                 rust-regex-1.12.3
                                 rust-regex-automata-0.4.14
                                 rust-regex-syntax-0.8.9
                                 rust-ring-0.17.14
                                 rust-rustls-0.22.4
                                 rust-rustls-pemfile-2.2.0
                                 rust-rustls-pki-types-1.14.0
                                 rust-rustls-webpki-0.102.8
                                 rust-rustversion-1.0.22
                                 rust-ryu-1.0.23
                                 rust-serde-1.0.228
                                 rust-serde-core-1.0.228
                                 rust-serde-derive-1.0.228
                                 rust-serde-json-1.0.99
                                 rust-serde-spanned-1.0.4
                                 rust-scopeguard-1.2.0
                                 rust-sha1-0.10.6
                                 rust-sharded-slab-0.1.7
                                 rust-shell-words-1.1.1
                                 rust-shellexpand-3.1.2
                                 rust-shlex-1.3.0
                                 rust-signal-hook-registry-1.4.8
                                 rust-simd-adler32-0.3.8
                                 rust-slab-0.4.12
                                 rust-smallvec-1.15.1
                                 rust-socket2-0.6.2
                                 rust-strsim-0.11.1
                                 rust-subtle-2.6.1
                                 rust-syn-2.0.117
                                 ;; rust-thiserror-2.0.17
                                 rust-tinyvec-1.10.0
                                 rust-tinyvec-macros-0.1.1
                                 rust-thiserror-1.0.69
                                 rust-thiserror-impl-1.0.69
                                 rust-thread-local-1.1.9
                                 rust-tokio-1.49.0
                                 rust-tokio-macros-2.6.0
                                 rust-tokio-rustls-0.25.0
                                 rust-tokio-util-0.7.18
                                 rust-toml-0.9.8
                                 rust-toml-datetime-0.7.5+spec-1.1.0
                                 rust-toml-parser-1.0.8+spec-1.1.0
                                 rust-toml-writer-1.0.6+spec-1.1.0
                                 rust-tower-service-0.3.3
                                 rust-tracing-0.1.44
                                 rust-tracing-core-0.1.36
                                 rust-tracing-log-0.2.0
                                 rust-tracing-attributes-0.1.31
                                 rust-tracing-subscriber-0.3.22
                                 rust-try-lock-0.2.5
                                 rust-typenum-1.19.0
                                 rust-urlencoding-2.1.3
                                 rust-unicase-2.9.0
                                 rust-unicode-bidi-0.3.18
                                 rust-unicode-ident-1.0.24
                                 rust-unicode-joining-type-0.7.0
                                 rust-unicode-normalization-0.1.25
                                 rust-unicode-width-0.2.2
                                 rust-untrusted-0.9.0
                                 rust-url-2.5.8
                                 rust-utf8parse-0.2.2
                                 rust-valuable-0.1.1
                                 rust-version-check-0.9.5
                                 rust-want-0.3.1
                                 rust-wasi-0.11.1+wasi-snapshot-preview1
                                 rust-wasm-bindgen-0.2.111
                                 rust-wasm-bindgen-macro-0.2.111
                                 rust-wasm-bindgen-macro-support-0.2.111
                                 rust-wasm-bindgen-shared-0.2.111
                                 rust-windows-core-0.62.2
                                 rust-windows-implement-0.60.2
                                 rust-windows-interface-0.59.3
                                 rust-windows-link-0.2.1
                                 rust-windows-result-0.4.1
                                 rust-windows-strings-0.5.1
                                 rust-windows-sys-0.48.0
                                 rust-windows-sys-0.52.0
                                 rust-windows-sys-0.60.2
                                 rust-windows-sys-0.61.2
                                 ;; rust-windows-targets-0.48.1
                                 rust-windows-targets-0.52.6
                                 rust-windows-targets-0.53.5
                                 rust-windows-aarch64-gnullvm-0.48.5
                                 rust-windows-aarch64-gnullvm-0.52.6
                                 rust-windows-aarch64-gnullvm-0.53.1
                                 rust-windows-aarch64-msvc-0.48.5
                                 rust-windows-aarch64-msvc-0.52.6
                                 rust-windows-aarch64-msvc-0.53.1
                                 rust-windows-i686-gnu-0.48.5
                                 rust-windows-i686-gnu-0.52.6
                                 rust-windows-i686-gnu-0.53.1
                                 rust-windows-i686-gnullvm-0.52.6
                                 rust-windows-i686-gnullvm-0.53.1
                                 rust-windows-i686-msvc-0.48.5
                                 rust-windows-i686-msvc-0.52.6
                                 rust-windows-i686-msvc-0.53.1
                                 rust-windows-x86-64-gnu-0.48.5
                                 rust-windows-x86-64-gnu-0.52.6
                                 rust-windows-x86-64-gnu-0.53.1
                                 rust-windows-x86-64-gnullvm-0.48.5
                                 rust-windows-x86-64-gnullvm-0.52.6
                                 rust-windows-x86-64-gnullvm-0.53.1
                                 rust-windows-x86-64-msvc-0.48.5
                                 rust-windows-x86-64-msvc-0.52.6
                                 rust-windows-x86-64-msvc-0.53.1
                                 rust-winnow-0.7.13
                                 rust-utf8-iter-1.0.4
                                 rust-zeroize-1.8.2)))


