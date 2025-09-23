;;; llm/git-heatmap/init.el -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'magit-log)

(defun git-heatmap-rank-files (&optional since-period max-results directory)
  "Rank files in project that contains DIRECTORY by git commit frequency using magit.

SINCE-PERIOD defaults to '3.month' (git log --since format).
MAX-RESULTS defaults to 100 for number of top files to return.
DIRECTORY defaults to current directory."
  (interactive)
  (unless (require 'magit nil t)
    (error "Magit is required for this function"))
  (let* ((dir (or directory default-directory))
         (since (or since-period "3.month"))
         (limit (or max-results 100))
         (dir (if (fboundp 'doom-project-root)
                  (doom-project-root dir)
                (or (magit-toplevel dir) dir)))
         (default-directory dir))
    (unless (magit-git-repo-p dir)
      (error "Not a git repository: %s" dir))
    (let* ((files (magit-git-lines "log"
                                   (format "--since=%s" since)
                                   "--pretty=format:"
                                   "--name-only"
                                   "--no-merges"))
           (file-counts (make-hash-table :test 'equal))
           (results '()))
      ;; Count file occurrences
      (dolist (file files)
        (unless (string-empty-p file)
          (puthash file (1+ (gethash file file-counts 0)) file-counts)))
      ;; Convert to sorted list
      (maphash (lambda (file count)
                 (push (cons count file) results))
               file-counts)
      ;; Sort by count (descending) and limit results
      (seq-take (sort results (lambda (a b) (> (car a) (car b)))) limit))))

(comment
  (git-heatmap-rank-files)
  (git-heatmap-rank-files "1.year")
  (git-heatmap-rank-files "2.year" 3))

;;; init.el ends here
