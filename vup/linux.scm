;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (vup linux)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages nfs)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix download))

(define (linux-nonfree-urls version)
  "Return a list of URLs for Linux-Nonfree VERSION."
  (list (string-append
         "https://www.kernel.org/pub/linux/kernel/"
         "v" (version-prefix version 1) ".x/"
         "linux-" version ".tar.xz")))

(define-public linux-nonfree
  (let* ((version "5.19"))
    (package
      (inherit linux-libre)
      (name "linux-nonfree")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (linux-nonfree-urls version))
                (sha256
                 (base32
                  "1a05a3hw4w3k530mxhns96xw7hag743xw5w967yazqcykdbhq97z"))))
      (synopsis "Mainline Linux kernel, nonfree binary blobs included.")
      (description "Linux is a kernel.")
      (license license:gpl2)
      (home-page "https://kernel.org/"))))

(define linux-firmware-version "20220708")
(define (linux-firmware-source version)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url (string-append "https://git.kernel.org/pub/scm/linux/kernel"
                              "/git/firmware/linux-firmware.git"))
          (commit version)))
    (file-name (git-file-name "linux-firmware" (string-take version 8)))
    (sha256
     (base32
      "10ghb0r0hd3pv5ggb4i51i7ri7ml755rddhs91r70i1q2h3rpjcf"))))

(define-public radeon-firmware-nonfree
  (package
    (name "radeon-firmware-nonfree")
    (version linux-firmware-version)
    (source (linux-firmware-source version))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((source (assoc-ref %build-inputs "source"))
                         (fw-dir (string-append %output "/lib/firmware/radeon/")))
                     (mkdir-p fw-dir)
                     (for-each (lambda (file)
                                 (copy-file file
                                            (string-append fw-dir "/"
                                                           (basename file))))
                               (find-files source
                                           (lambda (file stat)
                                             (string-contains file "radeon"))))
                     #t))))
    (home-page "")
    (synopsis "Non-free firmware for Radeon integrated chips")
    (description "Non-free firmware for Radeon integrated chips")
    ;; FIXME: What license?
    (license (license:non-copyleft "https://git.kernel.org/?p=linux/kernel/git/firmware/linux-firmware.git;a=blob_plain;f=LICENCE.radeon_firmware;hb=HEAD"))))

(define-public ath10k-firmware-nonfree
  (package
    (name "ath10k-firmware-nonfree")
    (version linux-firmware-version)
    (source (linux-firmware-source version))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((source (assoc-ref %build-inputs "source"))
                         (fw-dir (string-append %output "/lib/firmware/")))
                     (mkdir-p fw-dir)
                     (copy-recursively (string-append source "/ath10k")
                                       (string-append fw-dir "/ath10k"))
                     #t))))
    (home-page "")
    (synopsis "Non-free firmware for ath10k wireless chips")
    (description "Non-free firmware for ath10k integrated chips")
    ;; FIXME: What license?
    (license (license:non-copyleft "https://git.kernel.org/?p=linux/kernel/git/firmware/linux-firmware.git;a=blob_plain;f=LICENCE.radeon_firmware;hb=HEAD"))))

(define-public linux-firmware-nonfree
  (package
    (name "linux-firmware-nonfree")
    (version linux-firmware-version)
    (source (linux-firmware-source version))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((source (assoc-ref %build-inputs "source"))
                         (fw-dir (string-append %output "/lib/firmware/")))
                     (mkdir-p fw-dir)
                     (copy-recursively source fw-dir)
                     #t))))
    (home-page "")
    (synopsis "Non-free firmware for Linux")
    (description "Non-free firmware for Linux")
    ;; FIXME: What license?
    (license (license:non-copyleft "https://git.kernel.org/?p=linux/kernel/git/firmware/linux-firmware.git;a=blob_plain;f=LICENCE.radeon_firmware;hb=HEAD"))))

(define-public perf-nonfree
  (package
    (inherit perf)
    (name "perf-nonfree")
    (version (package-version linux-nonfree))
    (source (package-source linux-nonfree))
    (license (package-license linux-nonfree))))

(define-public cpupower-nonfree
  (package
    (inherit cpupower)
    (name "cpupower-nonfree")
    (version (package-version linux-nonfree))
    (source (package-source linux-nonfree))
    (license (package-license linux-nonfree))))

(define-public iwlwifi-firmware-nonfree
  (package
    (name "iwlwifi-firmware-nonfree")
    (version linux-firmware-version)
    (source (linux-firmware-source version))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((source (assoc-ref %build-inputs "source"))
                         (fw-dir (string-append %output "/lib/firmware")))
                     (mkdir-p fw-dir)
                     (for-each (lambda (file)
                                 (copy-file file
                                            (string-append fw-dir "/"
                                                           (basename file))))
                               (find-files source "iwlwifi-.*\\.ucode$|LICENCE\\.iwlwifi_firmware$"))
                     #t))))
    (home-page "https://wireless.wiki.kernel.org/en/users/drivers/iwlwifi")
    (synopsis "Non-free firmware for Intel wifi chips")
    (description "Non-free firmware for Intel wifi chips")
    ;; FIXME: What license?
    (license (license:non-copyleft "https://git.kernel.org/?p=linux/kernel/git/firmware/linux-firmware.git;a=blob_plain;f=LICENCE.iwlwifi_firmware;hb=HEAD"))))

(define-public ibt-hw-firmware-nonfree
  (package
    (name "ibt-hw-firmware-nonfree")
    (version linux-firmware-version)
    (source (linux-firmware-source version))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((source (assoc-ref %build-inputs "source"))
                         (fw-dir (string-append %output "/lib/firmware/intel")))
                     (mkdir-p fw-dir)
                     (for-each (lambda (file)
                                 (copy-file file
                                            (string-append fw-dir "/"
                                                           (basename file))))
                               (find-files source "ibt-hw-.*\\.bseq$|LICENCE\\.ibt_firmware$"))
                     #t))))
    (home-page "https://www.intel.com/support/wireless/wlan/sb/CS-016675.htm")
    (synopsis "Non-free firmware for Intel bluetooth chips")
    (description "Non-free firmware for Intel bluetooth chips")
    ;; FIXME: What license?
    (license (license:non-copyleft "https://git.kernel.org/?p=linux/kernel/git/firmware/linux-firmware.git;a=blob_plain;f=LICENCE.ibt_firmware;hb=HEAD"))))

; (define-public zfs
;   (package
;     (name "zfs")
;     (version "2.1.3")
;     (outputs '("out" "module" "src"))
;     (source
;       (origin
;         (method url-fetch)
;         (uri (string-append "https://github.com/openzfs/zfs/releases"
;                             "/download/zfs-" version
;                             "/zfs-" version ".tar.gz"))
;         (sha256
;          (base32 "1za3kf67qic210s8hgh1rnq74bwgsx98llynmyf40gvr8x2n86xn"))))
;     (build-system linux-module-build-system)
;     (arguments
;      `(;; The ZFS kernel module should not be downloaded since the license
;        ;; terms don't allow for distributing it, only building it locally.
;        #:substitutable? #f
;        ;; Tests cannot run in an unprivileged build environment.
;        #:tests? #f
;        #:phases
;        (modify-phases %standard-phases
;          (add-after 'configure 'really-configure
;            (lambda* (#:key outputs inputs #:allow-other-keys)
;              (let ((out (assoc-ref outputs "out")))
;                (substitute* "configure"
;                  (("-/bin/sh") (string-append "-" (which "sh"))))
;                (invoke "./configure"
;                        "--with-config=all"
;                        (string-append "--prefix=" out)
;                        (string-append "--with-dracutdir=" out "/lib/dracut")
;                        (string-append "--with-udevdir=" out "/lib/udev")
;                        (string-append "--with-mounthelperdir=" out "/sbin")
;                        (string-append "--with-linux="
;                                       (assoc-ref inputs "linux-module-builder")
;                                       "/lib/modules/build")))))
;          (add-after 'unpack 'patch-source
;            (lambda* (#:key inputs outputs #:allow-other-keys)
;              (let ((out        (assoc-ref outputs "out"))
;                    (src        (assoc-ref outputs "src"))
;                    (util-linux (assoc-ref inputs "util-linux"))
;                    (nfs-utils  (assoc-ref inputs "nfs-utils"))
;                    (kmod       (assoc-ref inputs "kmod-runtime")))
;                ;; New feature "compatibility=" in 2.1.0.
;                ;; This feature looks up in two locations:
;                ;;   /etc/zfs/compatibility.d/
;                ;;   /usr/share/zfs/compatibility.d/
;                ;; The first is intended for system-specific compatibility
;                ;; sets, while the second is what is installed with the
;                ;; OpenZFS package, so use the absolute path for the first
;                ;; (which requires patching in the file) and the store path
;                ;; for the second (which it gets by default).
;                (substitute* "include/sys/fs/zfs.h"
;                  (("#define\tZPOOL_SYSCONF_COMPAT_D.*$")
;                   ; Use absolute path.
;                   "#define\tZPOOL_SYSCONF_COMPAT_D\t\"/etc/zfs/compatibility.d\"\n"))
;                ;; Also update the manual, which uses absolute paths, so that
;                ;; /usr/share/zfs/compatibility.d/ is referred via the store.
;                (substitute* '("man/man7/zpoolprops.7"
;                               "man/man7/zpool-features.7")
;                  (("/usr/share/zfs/compatibility.d")
;                   (string-append out "/share/zfs/compatibility.d")))
;                (substitute* "etc/Makefile.in"
;                  ;; This just contains an example configuration file for
;                  ;; configuring ZFS on traditional init systems, skip it
;                  ;; since we cannot use it anyway; the install target becomes
;                  ;; misdirected.
;                  (("= default ") "= "))
;                (substitute* "lib/libzfs/os/linux/libzfs_util_os.c"
;                  ;; Use path to /gnu/store/*-kmod in actual path that is exec'ed.
;                  (("\"/sbin/modprobe\"")
;                   (string-append "\"" kmod "/bin/modprobe" "\""))
;                  ;; Just use 'modprobe' in message to user, since Guix
;                  ;; does not have a traditional /sbin/
;                  (("'/sbin/modprobe ") "'modprobe "))
;                (substitute* "contrib/Makefile.in"
;                  ;; This is not configurable nor is its hard-coded /usr prefix.
;                  ((" initramfs") ""))
;                (substitute* "module/os/linux/zfs/zfs_ctldir.c"
;                  (("/usr/bin/env\", \"umount")
;                   (string-append util-linux "/bin/umount\", \"-n"))
;                  (("/usr/bin/env\", \"mount")
;                   (string-append util-linux "/bin/mount\", \"-n")))
;                (substitute* "lib/libzfs/os/linux/libzfs_mount_os.c"
;                  (("/bin/mount") (string-append util-linux "/bin/mount"))
;                  (("/bin/umount") (string-append util-linux "/bin/umount")))
;                (substitute* "lib/libshare/os/linux/nfs.c"
;                  (("/usr/sbin/exportfs")
;                   (string-append nfs-utils "/sbin/exportfs")))
;                (substitute* "config/zfs-build.m4"
;                  (("\\$sysconfdir/init.d") (string-append out "/etc/init.d")))
;                (substitute* '("etc/zfs/Makefile.am"
;                               "cmd/zed/Makefile.am")
;                  (("\\$\\(sysconfdir)") (string-append out "/etc")))
;                (substitute* "cmd/vdev_id/vdev_id"
;                  (("PATH=/bin:/sbin:/usr/bin:/usr/sbin")
;                   (string-append "PATH="
;                                  (dirname (which "chmod")) ":"
;                                  (dirname (which "grep")) ":"
;                                  (dirname (which "sed")) ":"
;                                  (dirname (which "gawk")))))
;                (substitute* "contrib/pyzfs/Makefile.in"
;                  ((".*install-lib.*") ""))
;                (substitute* '("Makefile.am" "Makefile.in")
;                  (("\\$\\(prefix)/src") (string-append src "/src")))
;                (substitute* (find-files "udev/rules.d/" ".rules.in$")
;                  (("/sbin/modprobe") (string-append kmod "/bin/modprobe"))))
;              #t))
;          (replace 'build
;            (lambda _ (invoke "make")))
;          (replace 'install
;            (lambda* (#:key outputs inputs native-inputs #:allow-other-keys)
;              (let* ((out    (assoc-ref outputs "out"))
;                     (moddir (assoc-ref outputs "module"))
;                     (kmod   (assoc-ref (or native-inputs inputs) "kmod")))
;                (invoke "make" "install"
;                        (string-append "DEFAULT_INITCONF_DIR=" out "/etc/default")
;                        (string-append "DEPMOD=" kmod "/bin/depmod")
;                        (string-append "INSTALL_PATH=" out)
;                        (string-append "INSTALL_MOD_PATH=" moddir)
;                        "INSTALL_MOD_STRIP=1")
;                (install-file "contrib/bash_completion.d/zfs"
;                              (string-append out "/share/bash-completion/completions"))
;                #t))))))
;     (native-inputs
;      `(("attr" ,attr)
;        ("kmod" ,kmod)
;        ("pkg-config" ,pkg-config)))
;     (inputs
;      `(("eudev" ,eudev)
;        ("kmod-runtime" ,kmod)
;        ("libaio" ,libaio)
;        ("libtirpc" ,libtirpc)
;        ("nfs-utils" ,nfs-utils)
;        ("openssl" ,openssl)
;        ("python" ,python)
;        ("python-cffi" ,python-cffi)
;        ("util-linux" ,util-linux)
;        ("util-linux:lib" ,util-linux "lib")
;        ("zlib" ,zlib)))
;     (home-page "https://zfsonlinux.org/")
;     (synopsis "OpenZFS on Linux")
;     (description
;      "OpenZFS is an advanced file system and volume manager which was
; originally developed for Solaris and is now maintained by the OpenZFS
; community.")
;     (license license:cddl1.0)))

; (define-public zfs-auto-snapshot
;   (package
;     (name "zfs-auto-snapshot")
;     (version "1.2.4")
;     (source
;      (origin
;        (method git-fetch)
;        (uri (git-reference
;              (url
;               (string-append "https://github.com/zfsonlinux/" name))
;              (commit
;               (string-append "upstream/" version))))
;        (file-name (git-file-name name version))
;        (sha256
;         (base32 "0m4xw7h5qlbn5zdf9wb137pcr5l7hyrr7w2dgr16dfm5ay64vvfq"))))
;     (build-system gnu-build-system)
;     (inputs
;      ;; Note: if you are inheriting from the above zfs package in order
;      ;; to provide a specific stable kernel version, you should also
;      ;; inherit this package and replace the sole input below.
;      `(("zfs" ,zfs)))
;     (arguments
;      `(#:tests? #f ; No tests
;        #:phases
;        (modify-phases %standard-phases
;          (delete 'configure)
;          (delete 'build)
;          ;; Guix System may not have a traditional cron system, but
;          ;; the cron scripts installed by this package are convenient
;          ;; to use as targets for an mcron job specification, so make
;          ;; sure they can be run in-store.
;          (add-before 'install 'fix-scripts
;            (lambda* (#:key outputs inputs #:allow-other-keys)
;              (let* ((out                (assoc-ref outputs "out"))
;                     (zfs-auto-snapshot  (string-append
;                                          out
;                                          "/sbin/zfs-auto-snapshot"))
;                     (zfs-package        (assoc-ref inputs "zfs"))
;                     (zpool              (string-append
;                                          zfs-package
;                                          "/sbin/zpool"))
;                     (zfs                (string-append
;                                          zfs-package
;                                          "/sbin/zfs")))
;                (substitute* '("etc/zfs-auto-snapshot.cron.daily"
;                               "etc/zfs-auto-snapshot.cron.frequent"
;                               "etc/zfs-auto-snapshot.cron.hourly"
;                               "etc/zfs-auto-snapshot.cron.monthly"
;                               "etc/zfs-auto-snapshot.cron.weekly")
;                  (("zfs-auto-snapshot")
;                   zfs-auto-snapshot))
;                (substitute* "src/zfs-auto-snapshot.sh"
;                  (("LC_ALL=C zfs list")
;                   (string-append "LC_ALL=C " zfs " list"))
;                  (("LC_ALL=C zpool status")
;                   (string-append "LC_ALL=C " zpool " status"))
;                  (("zfs snapshot")
;                   (string-append zfs " snapshot"))
;                  (("zfs destroy")
;                   (string-append zfs " destroy"))))))
;          ;; Provide DESTDIR and PREFIX on make command.
;          (replace 'install
;            (lambda* (#:key outputs #:allow-other-keys)
;              (let ((out (assoc-ref outputs "out")))
;                (invoke "make" "install"
;                        "PREFIX="
;                        (string-append "DESTDIR=" out)))
;              #t)))))
;     (home-page "https://github.com/zfsonlinux/zfs-auto-snapshot")
;     (synopsis "Automatically create, rotate and destroy ZFS snapshots")
;     (description "An alternative implementation of the zfs-auto-snapshot
; service for Linux that is compatible with zfs-linux (now OpenZFS) and
; zfs-fuse.
;
; On Guix System, you will need to invoke the included shell scripts as
; @code{job} definitions in your @code{operating-system} declaration.")
;     (license license:gpl2+)))
