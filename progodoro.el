;;; progodoro.el --- A progressive overload pomodoro system using 'org-timer'.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Daniel Lin

;; Author:  Daniel Lin <two-e-asteroid@outlook.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A progressive overload pomodoro system using 'org-timer'.

;;; Code:



(provide 'progodoro)
(require 'org-timer)
(require 'emacs-remind)

(defvar progodoro-countdown nil
  "Stores the current mode of the progodoro.
NIL means that the timer has been reset and that the baseline focus time hasn't
been recorded yet. 1 means that either the baseline focus time is being recorded
now, or has already been recorded. t means that the baseline focus time has
already been raised.")

(defvar progodoro-focus "00:00:00 "
  "The amount of time the progodoro has gone up to as of now.
It is stored as a string of format `HH:MM:SS `.")

(defvar progodoro-recharging-p nil
  "Whether the progodoro is recharging.")

(defvar progodoro-pity 0
  "Every non-long break that a person gets will increase the pity by an integer.
The integer will be between 0 and 5. Before every break, it will roll and if the
integer between 0 and (100 - pity) is 0, then it will be a long break and pity
will be reset.")

(defvar progodoro-break "00:05:00 ")

(defun discharge-progodoro ()
  "Discharges the timer (a.k.a. work).
The first time you run this, it'll be a step up timer which will test the current limit of your flow. After that, each timer will be progressively larger by 5 minutes. Breaks are at a constant of 'progodoro-break', but every so often, you will get a longer break."
  (interactive)
  (cond ((not progodoro-countdown) (progn (org-timer-start)
                                          (setq progodoro-countdown 1)
                                          (message "Starting forge.")))
        ((eq progodoro-countdown 1) (progn (setq progodoro-focus (org-timer-value-string))
                                           (org-timer-stop)
                                           (message "Progodoro Timer Forged.")
                                           (setq progodoro-countdown t)))
        (t (progn (let ((time-list (split-string progodoro-focus (rx ":"))))

                    (setq progodoro-focus (format "%s:%s:%s "
                                                  (string-to-number (car time-list))
                                                  (+ 5 (string-to-number (cadr time-list)))
                                                  (string-to-number (caddr time-list)))))
                  (org-timer-set-timer progodoro-focus)))))


(defun recharge-progodoro ()
  "Recharges the pomodoro (a.k.a. break)."
  (interactive)
  (let* ((capped-p (>= progodoro-pity 95))
         (roll (random (- 100 (if capped-p 95 progodoro-pity))))
         (effort (floor (/ (funcall
                            (lambda (self numbers add) (funcall self numbers add self))
                            (lambda (numbers add self) (if (null numbers) 0 (+ (* add (car numbers)) (funcall self (cdr numbers) (/ add 60) self))))
                            (mapcar #'string-to-number (split-string progodoro-focus ":"))
                            3600) 50)))
         (pity-increase (random effort)))
    (if (or (= roll 0) (> progodoro-pity 150))
        (progn (setq progodoro-recharging-p t)
               (org-timer-set-timer (+ 30 (random (if capped-p (+ 70 (- progodoro-pity 95)) 70))))
               (setq progodoro-pity 0)
               (message "You are given a long break!"))
      (progn (message (format "Due to your effort of %s, you are given an increase in pity of %s.\nYou rolled a %s." effort pity-increase roll))
             (setq progodoro-recharging-p t)
             (setq progodoro-pity (+ pity-increase progodoro-pity))
             (org-timer-set-timer progodoro-break)))))

(defun drain-progodoro ()
  "Drains the timer. It should probably be ran every start of the day."
  (interactive)
  (org-timer-stop)
  (setq progodoro-recharging-p nil)
  (setq progodoro-countdown nil)
  (setq progodoro-focus "00:00:00 "))

(add-hook 'org-timer-done-hook (lambda () (if progodoro-recharging-p
                                         (progn (emacs-remind "Your timer has recharged. You can proceed to discharge the timer again." "Progodoro")
                                                (setq progodoro-recharging-p nil))
                                       (emacs-remind "Your timer has overloaded. Please recharge your timer." "Progodoro"))))

(global-set-key (kbd "<f5>") 'discharge-progodoro)
(global-set-key (kbd "<f6>") 'recharge-progodoro)
(global-set-key (kbd "<f7>") 'org-timer-pause-or-continue)
(global-set-key (kbd "<f8>") 'drain-progodoro)

;;; progodoro.el ends here
