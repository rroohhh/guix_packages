;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2020 Jesse Dowell <jessedowell@gmail.com>
;;; Copyright © 2021 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2021 Robin Heinemann <robin.ole.heinemann@gmail.com>
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

(define-module (vup docker)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages version-control)
  #:use-module ((gnu packages docker) #:select (containerd))
  #:use-module (gnu packages virtualization))

;; Note - when changing Docker versions it is important to update the versions
;; of several associated packages (docker-libnetwork and go-sctp).
(define %docker-version "20.10.6")


;;; Private package that shouldn't be used directly; its purposes is to be
;;; used as a template for the various packages it contains.  It doesn't build
;;; anyway, as it needs many dependencies that aren't being satisfied.
(define docker-libnetwork
  ;; There are no recent release for libnetwork, so choose the last commit of
  ;; the branch that Docker uses, as can be seen in the Docker source file
  ;; 'hack/dockerfile/install/proxy.installer'. NOTE - It is important that
  ;; this version is kept in sync with the version of Docker being used.
  ;; This commit is the "bump_19.03" branch, as mentioned in Docker's vendor.conf.
  (let ((commit "b3507428be5b458cb0e2b4086b13531fb0706e46")
        (version (version-major+minor %docker-version))
        (revision "1"))
    (package
      (name "docker-libnetwork")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      ;; Redirected from github.com/docker/libnetwork.
                      (url "https://github.com/moby/libnetwork")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0aq1b0gqkpxky6w1a62lksxw0hn2skj9ajyn25sn7hra3xnvlp7r"))
                ;; Delete bundled ("vendored") free software source code.
                (modules '((guix build utils)))
                (snippet '(begin
                            (delete-file-recursively "vendor")
                            #t))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/moby/libnetwork/"))
      (home-page "https://github.com/moby/libnetwork/")
      (synopsis "Networking for containers")
      (description "Libnetwork provides a native Go implementation for
connecting containers.  The goal of @code{libnetwork} is to deliver a robust
container network model that provides a consistent programming interface and
the required network abstractions for applications.")
      (license license:asl2.0))))

(define-public docker-libnetwork-cmd-proxy
  (package
    (inherit docker-libnetwork)
    (name "docker-libnetwork-cmd-proxy")
    (arguments
     `(#:import-path "github.com/docker/libnetwork/cmd/proxy"
       #:unpack-path "github.com/docker/libnetwork"
       #:install-source? #f))
    (native-inputs
     `(("go-sctp" ,go-sctp)
       ;; For tests.
       ("logrus" ,go-github-com-sirupsen-logrus)
       ("go-netlink" ,go-netlink)
       ("go-netns" ,go-netns)
       ("go-golang-org-x-crypto"
        ,go-golang-org-x-crypto)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (synopsis "Docker user-space proxy")
    (description "A proxy running in the user space.  It is used by the
built-in registry server of Docker.")
    (license license:asl2.0)))

;; TODO: Patch out modprobes for ip_vs, nf_conntrack,
;; brige, nf_conntrack_netlink, aufs.
(define-public docker
  (package
    (name "docker")
    (version %docker-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moby/moby")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l4ra9bsvydaxd2fy7dgxp7ynpp0mrlwvcdhxiafw596559ab6qk"))))
       ;; (patches
       ;;  (search-patches "docker-fix-tests.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((guix build gnu-build-system)
        ((guix build go-build-system) #:prefix go:)
        (guix build union)
        (guix build utils))
       #:imported-modules
       (,@%gnu-build-system-modules
        (guix build union)
        (guix build go-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "builder/builder-next/executor_unix.go"
               (("CommandCandidates:.*runc.*")
                (string-append "CommandCandidates: []string{\""
                               (assoc-ref inputs "runc")
                               "/sbin/runc\"},\n")))
             (substitute* "vendor/github.com/containerd/go-runc/runc.go"
               (("DefaultCommand = .*")
                (string-append "DefaultCommand = \""
                               (assoc-ref inputs "runc")
                               "/sbin/runc\"\n")))
             (substitute* "vendor/github.com/containerd/containerd/runtime/v1/linux/runtime.go"
               (("defaultRuntime[ \t]*=.*")
                (string-append "defaultRuntime = \""
                               (assoc-ref inputs "runc")
                               "/sbin/runc\"\n"))
               (("defaultShim[ \t]*=.*")
                (string-append "defaultShim = \""
                               (assoc-ref inputs "containerd")
                               "/bin/containerd-shim\"\n")))
             (substitute* "daemon/daemon_unix.go"
               (("DefaultShimBinary = .*")
                (string-append "DefaultShimBinary = \""
                               (assoc-ref inputs "containerd")
                               "/bin/containerd-shim\"\n"))
               (("DefaultRuntimeBinary = .*")
                (string-append "DefaultRuntimeBinary = \""
                               (assoc-ref inputs "runc")
                               "/sbin/runc\"\n"))
               (("DefaultRuntimeName = .*")
                (string-append "DefaultRuntimeName = \""
                               (assoc-ref inputs "runc")
                               "/sbin/runc\"\n")))
             (substitute* "vendor/github.com/moby/buildkit/executor/runcexecutor/executor.go"
               (("defaultCommandCandidates = .*")
                (string-append "defaultCommandCandidates = []string{\""
                               (assoc-ref inputs "runc")
                               "/sbin/runc\"}\n")))
             (substitute* "vendor/github.com/containerd/containerd/runtime/v1/linux/runtime.go"
               (("defaultRuntime = .*")
                (string-append "defaultRuntime = \""
                               (assoc-ref inputs "runc")
                               "/sbin/runc\"\n")))
             (substitute* "daemon/runtime_unix.go"
               (("defaultRuntimeName = .*")
                (string-append "defaultRuntimeName = \""
                               (assoc-ref inputs "runc")
                               "/sbin/runc\"\n")))
             (substitute* "daemon/config/config.go"
               (("StockRuntimeName = .*")
                (string-append "StockRuntimeName = \""
                               (assoc-ref inputs "runc")
                               "/sbin/runc\"\n"))
               (("DefaultInitBinary = .*")
                (string-append "DefaultInitBinary = \""
                               (assoc-ref inputs "tini")
                               "/bin/tini\"\n")))
             (substitute* "daemon/config/config_common_unix_test.go"
               (("expectedInitPath: \"docker-init\"")
                (string-append "expectedInitPath: \""
                               (assoc-ref inputs "tini")
                               "/bin/tini\"")))
             (substitute* "vendor/github.com/moby/buildkit/executor/runcexecutor/executor.go"
               (("var defaultCommandCandidates = .*")
                (string-append "var defaultCommandCandidates = []string{\""
                               (assoc-ref inputs "runc") "/sbin/runc\"}")))
             (substitute* "vendor/github.com/docker/libnetwork/portmapper/proxy.go"
               (("var userlandProxyCommandName = .*")
                (string-append "var userlandProxyCommandName = \""
                               (assoc-ref inputs "docker-proxy")
                               "/bin/proxy\"\n")))
             (substitute* "pkg/archive/archive.go"
               (("string\\{\"xz")
                (string-append "string{\"" (assoc-ref inputs "xz") "/bin/xz")))
             ;; TODO: Remove when Docker proper uses v1.14.x to build
             (substitute* "registry/resumable/resumablerequestreader_test.go"
               (("I%27m%20not%20an%20url" all)
                (string-append "\"" all "\"")))
             ;; ;; TODO: Remove when Docker proper uses v1.14.x to build
             ;; (substitute* "vendor/gotest.tools/x/subtest/context.go"
             ;;   (("func \\(tc \\*testcase\\) Cleanup\\(" all)
             ;;    (string-append all "func()"))
             ;;   (("tc\\.Cleanup\\(" all)
             ;;    (string-append all "nil")))

             (let ((source-files (filter (lambda (name)
                                           (not (string-contains name "test")))
                                         (find-files "." "\\.go$"))))
               (let-syntax ((substitute-LookPath*
                             (syntax-rules ()
                               ((_ (source-text package relative-path) ...)
                                (substitute* source-files
                                  (((string-append "\\<exec\\.LookPath\\(\""
                                                   source-text
                                                   "\")"))
                                   (string-append "\""
                                                  (assoc-ref inputs package)
                                                  "/" relative-path
                                                  "\", error(nil)")) ...))))
                            (substitute-Command*
                             (syntax-rules ()
                               ((_ (source-text package relative-path) ...)
                                (substitute* source-files
                                  (((string-append "\\<(re)?exec\\.Command\\(\""
                                                   source-text
                                                   "\"") _ re?)
                                   (string-append (if re? re? "")
                                                  "exec.Command(\""
                                                  (assoc-ref inputs package)
                                                  "/" relative-path
                                                  "\"")) ...)))))
                 (substitute-LookPath*
                  ("containerd" "containerd" "bin/containerd")
                  ("ps" "procps" "bin/ps")
                  ("mkfs.xfs" "xfsprogs" "bin/mkfs.xfs")
                  ("lvmdiskscan" "lvm2" "sbin/lvmdiskscan")
                  ("pvdisplay" "lvm2" "sbin/pvdisplay")
                  ("blkid" "util-linux" "sbin/blkid")
                  ("unpigz" "pigz" "bin/unpigz")
                  ("iptables" "iptables" "sbin/iptables")
                  ("ip6tables" "iptables" "sbin/ip6tables")
                  ("iptables-legacy" "iptables" "sbin/iptables")
                  ("ip" "iproute2" "sbin/ip"))

                 (substitute-Command*
                  ("modprobe" "kmod" "bin/modprobe")
                  ("pvcreate" "lvm2" "sbin/pvcreate")
                  ("vgcreate" "lvm2" "sbin/vgcreate")
                  ("lvcreate" "lvm2" "sbin/lvcreate")
                  ("lvconvert" "lvm2" "sbin/lvconvert")
                  ("lvchange" "lvm2" "sbin/lvchange")
                  ("mkfs.xfs" "xfsprogs" "sbin/mkfs.xfs")
                  ("xfs_growfs" "xfsprogs" "sbin/xfs_growfs")
                  ("mkfs.ext4" "e2fsprogs" "sbin/mkfs.ext4")
                  ("tune2fs" "e2fsprogs" "sbin/tune2fs")
                  ("blkid" "util-linux" "sbin/blkid")
                  ("resize2fs" "e2fsprogs" "sbin/resize2fs")
                  ("ps" "procps" "bin/ps")
                  ("losetup" "util-linux" "sbin/losetup")
                  ("uname" "coreutils" "bin/uname")
                  ("dbus-launch" "dbus" "bin/dbus-launch")
                  ("git" "git" "bin/git")))
               ;; docker-mountfrom ??
               ;; docker
               ;; docker-untar ??
               ;; docker-applyLayer ??
               ;; /usr/bin/uname
               ;; grep
               ;; apparmor_parser

               ;; Make compilation fail when, in future versions, Docker
               ;; invokes other programs we don't know about and thus don't
               ;; substitute.
               (substitute* source-files
                 ;; Search for Java in PATH.
                 (("\\<exec\\.Command\\(\"java\"")
                  "xxec.Command(\"java\"")
                 ;; Search for AUFS in PATH (mainline Linux doesn't support it).
                 (("\\<exec\\.Command\\(\"auplink\"")
                  "xxec.Command(\"auplink\"")
                 ;; Fail on other unsubstituted commands.
                 (("\\<exec\\.Command\\(\"([a-zA-Z0-9][a-zA-Z0-9_-]*)\""
                   _ executable)
                  (string-append "exec.Guix_doesnt_want_Command(\""
                                 executable "\""))
                 (("\\<xxec\\.Command")
                  "exec.Command")
                 ;; Search for ZFS in PATH.
                 (("\\<LookPath\\(\"zfs\"\\)") "LooxPath(\"zfs\")")
                 ;; (("\\<LookPath\\(\"buildkit-qemu-\" \\+ a\\)") "LooxPath(\"buildkit-qemu-\" \\+ a)")
                 ;; Fail on other unsubstituted LookPaths.
                 ;; (("\\<LookPath\\(\"") "Guix_doesnt_want_LookPath\\(\"")
                 (("\\<LooxPath") "LookPath")))
             #t))
         (add-after 'patch-paths 'delete-failing-tests
           (lambda _
             ;; Needs internet access.
             (delete-file "builder/remotecontext/git/gitutils_test.go")
             ;; Permission denied.
             (delete-file "daemon/graphdriver/devmapper/devmapper_test.go")
             ;; Operation not permitted (idtools.MkdirAllAndChown).
             (delete-file "daemon/graphdriver/vfs/vfs_test.go")
             ;; Timeouts after 5 min.
             (delete-file "plugin/manager_linux_test.go")
             ;; Operation not permitted.
             (delete-file "daemon/graphdriver/aufs/aufs_test.go")
             (delete-file "daemon/graphdriver/btrfs/btrfs_test.go")
             (delete-file "daemon/graphdriver/overlay/overlay_test.go")
             (delete-file "daemon/graphdriver/overlay2/overlay_test.go")
             (delete-file "pkg/chrootarchive/archive_unix_test.go")
             (delete-file "daemon/container_unix_test.go")
             ;; This file uses cgroups and /proc.
             (delete-file "pkg/sysinfo/sysinfo_linux_test.go")
             ;; This file uses cgroups.
             (delete-file "runconfig/config_test.go")
             ;; This file uses /var.
             (delete-file "daemon/oci_linux_test.go")
             ;; Signal tests fail in bizarre ways
             (delete-file "pkg/signal/signal_linux_test.go")
             ;; tests fail in bizarre ways
             (delete-file "registry/resumable/resumablerequestreader_test.go")
             #t))
         (replace 'configure
           (lambda _
             (setenv "DOCKER_BUILDTAGS" "seccomp")
             (setenv "DOCKER_GITCOMMIT" (string-append "v" ,%docker-version))
             (setenv "VERSION" (string-append ,%docker-version "-ce"))
             ;; Automatically use bundled dependencies.
             ;; TODO: Unbundle - see file "vendor.conf".
             (setenv "AUTO_GOPATH" "1")
             ;; Respectively, strip the symbol table and debug
             ;; information, and the DWARF symbol table.
             (setenv "LDFLAGS" "-s -w")
             ;; Make build faster
             (setenv "GOCACHE" "/tmp")
             #t))
         (add-before 'build 'setup-go-environment
           (assoc-ref go:%standard-phases 'setup-go-environment))
         (replace 'build
           (lambda _
             ;; Our LD doesn't like the statically linked relocatable things
             ;; that go produces, so install the dynamic version of
             ;; dockerd instead.
             (invoke "hack/make.sh" "dynbinary")))
         (replace 'check
           (lambda _
             ;; The build process generated a file because the environment
             ;; variable "AUTO_GOPATH" was set.  Use it.
             (setenv "GOPATH" (string-append (getcwd) "/.gopath"))
             ;; ".gopath/src/github.com/docker/docker" is a link to the current
             ;; directory and chdir would canonicalize to that.
             ;; But go needs to have the uncanonicalized directory name, so
             ;; store that.
             (setenv "PWD" (string-append (getcwd)
                                          "/.gopath/src/github.com/docker/docker"))
             (with-directory-excursion ".gopath/src/github.com/docker/docker"
               (invoke "hack/test/unit"))
             (setenv "PWD" #f)
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-bin (string-append out "/bin")))
               (install-file "bundles/dynbinary-daemon/dockerd" out-bin)
               (install-file (string-append "bundles/dynbinary-daemon/dockerd-"
                                            (getenv "VERSION"))
                             out-bin)
               #t)))
         (add-after 'install 'remove-go-references
           (assoc-ref go:%standard-phases 'remove-go-references)))))
    (inputs
     `(("btrfs-progs" ,btrfs-progs)
       ("containerd" ,containerd)       ; for containerd-shim
       ("coreutils" ,coreutils)
       ("dbus" ,dbus)
       ("docker-proxy" ,docker-libnetwork-cmd-proxy)
       ("e2fsprogs" ,e2fsprogs)
       ("git" ,git)
       ("iproute2" ,iproute)
       ("iptables" ,iptables)
       ("kmod" ,kmod)
       ("libseccomp" ,libseccomp)
       ("pigz" ,pigz)
       ("procps" ,procps)
       ("runc" ,runc)
       ("util-linux" ,util-linux)
       ("lvm2" ,lvm2)
       ("tini" ,tini)
       ("xfsprogs" ,xfsprogs)
       ("xz" ,xz)))
    (native-inputs
     `(("eudev" ,eudev)      ; TODO: Should be propagated by lvm2 (.pc -> .pc)
       ("go" ,go)
       ("gotestsum" ,gotestsum)
       ("pkg-config" ,pkg-config)))
    (synopsis "Docker container component library, and daemon")
    (description "This package provides a framework to assemble specialized
container systems.  It includes components for orchestration, image
management, secret management, configuration management, networking,
provisioning etc.")
    (home-page "https://mobyproject.org/")
    (license license:asl2.0)))

(define-public docker-cli
  (package
    (name "docker-cli")
    (version %docker-version)
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/docker/cli")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "15kknb26vyzjgqmn8r81a1sy1i5br6bvngqd5xljihppnxvp2gvl"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/docker/cli"
       ;; TODO: Tests require a running Docker daemon.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'setup-environment-2
           (lambda _
             ;; Respectively, strip the symbol table and debug
             ;; information, and the DWARF symbol table.
             (setenv "LDFLAGS" "-s -w")

             ;; Make sure "docker -v" prints a usable version string.
             (setenv "VERSION" ,%docker-version)

             ;; Make build reproducible.
             (setenv "BUILDTIME" "1970-01-01 00:00:01.000000000+00:00")
             (symlink "src/github.com/docker/cli/scripts" "./scripts")
             (symlink "src/github.com/docker/cli/docker.Makefile" "./docker.Makefile")
             #t))
         (replace 'build
           (lambda _
             (invoke "./scripts/build/binary")))
         (replace 'check
           (lambda* (#:key make-flags tests? #:allow-other-keys)
             (setenv "PATH" (string-append (getcwd) "/build:" (getenv "PATH")))
             (if tests?
                 ;; Use the newly-built docker client for the tests.
                 (with-directory-excursion "src/github.com/docker/cli"
                   ;; TODO: Run test-e2e as well?
                   (apply invoke "make" "-f" "docker.Makefile" "test-unit"
                          (or make-flags '())))
                 #t)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-bin (string-append out "/bin"))
                    (etc (string-append out "/etc")))
               (with-directory-excursion "src/github.com/docker/cli/contrib/completion"
                 (install-file "bash/docker"
                               (string-append etc "/bash_completion.d"))
                 (install-file "fish/docker.fish"
                               (string-append etc "/fish/completions"))
                 (install-file "zsh/_docker"
                               (string-append etc "/zsh/site-functions")))
               (install-file "build/docker" out-bin)
               #t))))))
    (native-inputs
     `(("go" ,go)
       ("libltdl" ,libltdl)
       ("pkg-config" ,pkg-config)))
    (synopsis "Command line interface to Docker")
    (description "This package provides a command line interface to Docker.")
    (home-page "https://www.docker.com/")
    (license license:asl2.0)))

(define-public tini
  (package
    (name "tini")
    (version "0.19.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/krallin/tini")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hnnvjydg7gi5gx6nibjjdnfipblh84qcpajc08nvr44rkzswck4"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                    ;tests require a Docker daemon
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-static-build
                    ;; Disable the static build as it fails to install, with
                    ;; the error: "No valid ELF RPATH or RUNPATH entry exists
                    ;; in the file".
                    (lambda _
                      (substitute* "CMakeLists.txt"
                        ((".*tini-static.*") ""))
                      #t)))))
    (home-page "https://github.com/krallin/tini")
    (synopsis "Tiny but valid init for containers")
    (description "Tini is an init program specifically designed for use with
containers.  It manages a single child process and ensures that any zombie
processes produced from it are reaped and that signals are properly forwarded.
Tini is integrated with Docker.")
    (license license:expat)))
