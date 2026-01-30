;;; build-binaries.el --- GitHub Actions workflow for building eldc binaries

;;; Commentary:
;; This file demonstrates eldc's ability to convert Emacs Lisp to YAML.
;; Uses plist format for clean, unambiguous JSON/YAML generation.
;;
;; Usage:
;;   1. Open this file in Emacs
;;   2. M-x load-file RET eldc.el RET (from project root)
;;   3. M-x eldc-yaml (creates build-binaries.yaml)
;;
;; The generated YAML will be functionally identical to the manually written workflow.
;;
;; Note: For complex nested structures, plists are preferred over alists because
;; they encode more predictably to JSON objects. Use vectors [...] for arrays.

;;; Code:

'(:name "Build Binaries"

  :on (:push (:tags ["v*"])
       :workflow_dispatch nil)

  :permissions (:contents "write")

  :jobs (:build (:name "Build Linux binary"
                 :runs-on "ubuntu-latest"
                 :steps [(:name "Checkout code"
                          :uses "actions/checkout@v4")

                         (:name "Setup Rust"
                          :uses "dtolnay/rust-toolchain@stable")

                         (:name "Build binary"
                          :run "cargo build --release --manifest-path eldc-j2y/Cargo.toml")

                         (:name "Rename binary"
                          :run "mv eldc-j2y/target/release/eldc-j2y eldc-j2y/target/release/eldc-j2y-linux-x86_64")

                         (:name "Upload artifact"
                          :uses "actions/upload-artifact@v4"
                          :with (:name "eldc-j2y-linux-x86_64"
                                 :path "eldc-j2y/target/release/eldc-j2y-linux-x86_64"))])

         :release (:name "Create Release"
                   :needs "build"
                   :runs-on "ubuntu-latest"
                   :if "startsWith(github.ref, 'refs/tags/')"
                   :steps [(:name "Download all artifacts"
                            :uses "actions/download-artifact@v4"
                            :with (:path "artifacts"))

                           (:name "Display structure of downloaded files"
                            :run "ls -R artifacts")

                           (:name "Create Release"
                            :uses "softprops/action-gh-release@v1"
                            :with (:files "artifacts/eldc-j2y-linux-x86_64/eldc-j2y-linux-x86_64"
                                   :draft :json-false
                                   :prerelease :json-false
                                   :generate_release_notes t)
                            :env (:GITHUB_TOKEN "${{ secrets.GITHUB_TOKEN }}"))])))

;;; build-binaries.el ends here
