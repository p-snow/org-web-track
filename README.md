<a href="https://melpa.org/#/org-web-track"><img alt="MELPA" src="https://melpa.org/packages/org-web-track-badge.svg"/></a>

<a href="https://stable.melpa.org/#/org-web-track"><img alt="MELPA Stable" src="https://stable.melpa.org/packages/org-web-track-badge.svg"/></a>

Org Web Track provides an Org Mode framework that assists users in managing their values of interest on a website or via a Web API.


# Overview

Through the use of Org Web Track:

-   Any location in the web page or any piece in the web API response can be an item to track
-   Users can monitor changes for the item and manage them using the facilities of Org mode
-   A set of items to track can be reviewed with their updated values in an Org column view
-   Users can extract an Org table displaying value changes and create a visual graph using Gnuplot.

<img src="https://github.com/p-snow/org-web-track/blob/main/images/org-web-track-columns_01.png?raw=true">

<img src="https://github.com/p-snow/org-web-track/blob/main/images/org-web-track-graph_01.png?raw=true">


## Retrieving data

When users specify which snippet of data to track on web pages or web APIs, the following methods are available:

-   CSS selectors, which are applied against the source code of the web page
-   Elisp functions, which receive a DOM object or a JSON object from a web page or a web API and are expected to return a desired value from the argument
-   Shell commands, which take a page source or an API response as STDIN and are expected to print a desired value to STDOUT


# Installation

Requirements:

-   Emacs 29.1 or higher
-   [request.el](https://github.com/tkf/emacs-request) v0.3.2
-   [enlive](https://github.com/zweifisch/enlive) v0.0.1


## MELPA package

Org Web Track can be installed from MELPA using package.el as follows:

1.  Add melpa to package-archives
    
        (add-to-list 'package-archives
                     '("melpa" . "https://melpa.org/packages/")
                     t)
2.  Refresh package contents
    
        M-x package-refresh-contents
3.  Install Org Web Track
    
        M-x package-install
    
    and select org-web-track


# Basic Usage

In this section, the basic Org Web Track mechanism and fundamental command usage are described.


## Defining Tracking Items

Before setting up each tracking item, users need to define `org-web-track-selectors-alist`, whose element consists of two components: URL-MATCH and SELECTOR. Each element is responsible for selecting a value for the specific URL that matches URL-MATCH, a regexp. The SELECTOR will then be applied to the content of the HTTP response at the URL to extract the resulting value.

A single URL-MATCH is associated with one or more SELECTORS. Therefore, the element of the variable appears as either (URL-MATCH . SELECTOR) or (URL-MATCH SELECTORS&#x2026;). Below is a typical example of how to set the variable:

(setq org-web-track-selectors-alist &rsquo;((&ldquo;example\\\\.com/product&rdquo; . [.final-price])))

This code define how to extract product prices at the site &ldquo;example.com&rdquo;. In the site, the price posits in the tag whose class is &ldquo;final-price&rdquo;. This case, SELECTOR is CSS selector, one of available formats. SELECTOR can be more than that. Please refer to the documentation of `org-web-track-selectors-alist` for more detail.

This code dictates how to extract product prices from the site &ldquo;example.com&rdquo;. On the site, the price is located within a tag whose class is &ldquo;final-price&rdquo;. In this case, SELECTOR is a vector, which signifies a CSS selector. Besides this, SELECTOR can also be a function or a string. This means it can represent an Elisp procedure or a shell command respectively. For more details, please refer to the documentation of `org-web-track-selectors-alist`.

After appropriately defining `org-web-track-selectors-alist`, users can set up each specific tracking item by calling `org-web-track-setup-entry` on the desired org entry or before the first heading to create a new one in the org buffer. Users will be prompted to input the tracking URL, and then the updated value will be retrieved, stored, and displayed in the echo area according to the aforementioned settings.

    * Book ABC
    :PROPERTIES:
    :TRACK_URL: https://example.com/products/book-abc.html
    :TRACK_CURRENT_VALUE: $30
    :TRACK_LAST_UPDATED_TIME: [2024-07-18 Thu 16:57]
    :END:
    :LOGBOOK:
    - Update "$30"       [2024-07-18 Thu 16:57]
    :END:

Some practical tracking item examples for specific real services are showcased on the Wiki page of the project website at <https://github.com/p-snow/org-web-track/wiki>.


### Sending Additional HTTP Headers

In an HTTP request message, a client can include a set of information known as an HTTP header, each element presented in a key-value pair format. This allows the client to send metadata to the server for various purposes, such as authentication or user identification.

In Org Web Track, users can specify HTTP headers in a couple of ways. First, they can set a variable `org-web-track-default-http-headers` that is applied to every HTTP request. Second, they can execute a command `org-web-track-set-http-headers` that sets specific HTTP headers for individual entries.


### Dealing with Dinamic Website

The HTTP request backend of org-web-track, either cURL or url-retrieve, lacks the ability to read a dynamic website like a JS-rendered page. To work around this issue, org-web-track employs a delegation mechanism for fetching content. CONTENT-FETCHER in the org-web-track-content-fetcher-alist is used for this purpose. Users are free to implement any procedure using libraries such as Selenium or WebDriver in it.


### Accessing Unix Domain Socket Server

While org-web-track primarily focuses on the WWW server as the access target, users also have the option to connect to a Unix Domain Socket server, which provides HTTP services mainly on a local machine. A simple example of a Unix Socket server implementation complying with the org-web-track framework can be found at <https://github.com/p-snow/socket-http-server>. Using this feature, non-HTTP services, such as Unix shell commands, can be effectively utilized within the org-web-track framework.

Users who want to access the Unix Domain Socket server must set the socket&rsquo;s path by calling `org-web-track-set-unix-socket` at the desired tracking item. When this feature is active, the value of the variable `org-web-track-use-curl` will not be respected. The cURL program will implicitly be used as the fetching backend since url-retrieve cannot access the Unix Socket Server.


## Updating Values

The simplest way to update the value is to call `org-web-track-update-entry` on the desired org entry. If the retrieved value is updated compared to the last value, the updated value will be stored as the TRACK\_CURRENT\_VALUE org property; otherwise, the entry will remain unchanged.

Alternatively, bulk updating is supported. To enable bulk updating, users must first define `org-web-track-files`. This variable should be a list of files in which all tracking items, identified by having the TRACK\_URL property, are selected for bulk updating. To perform bulk updating, call `org-web-track-update-files`.


## Displaying Column View

Column view in org-mode is a feature that displays properties in a table, providing a clear and comprehensive perspective. org-web-track offers a specialized column view where updated values are juxtaposed with their previous values for tracking items. To display the column view, call `org-web-track-columns` in org buffer.

If tracking items are scattered across many files, `org-web-track-agenda-columns` is useful as all tracking items in the aforementioned `org-web-track-files` are gathered in the agenda column view. Users can also update any item in the agenda column view by calling `org-web-track-agenda-update`.


## Reporting and Creating Graph

All updated values from the past are logged in the entry using the existing org log note feature. Log notes have a fixed format and are placed in a drawer only if `org-log-into-drawer` is non-nil.

`org-web-track-report` creates an Org table where all log note values in the current Org entry are listed in ascending order of time, showing the transition of values over time. If called with C-u prefix, the command creates a visual graph using Gnuplot. Note that creating a graph requires the Gnuplot Emacs package and Gnuplot itself.


# Extended Examples

In this section, examples of how to utilize `org-web-track` extensively are showcased.


## Automatic Bulk Updating and Email Notifications

While automatic updating may be ideal in certain situations, Org Web Track refrains from providing this feature directly to prevent potential data violations. However, users can enable automatic updating by calling `org-web-track-update-entry` or `org-web-track-update-files` from Elisp code. Below is an example implementation of automatic updates with email notifications scheduled for midnight.

    (defun exp/email-updated ()
      "Check for updates on all tracking items in `org-web-track-files'
    and email me the updated list of items formatted as requested."
      (let* ((message-kill-buffer-on-exit t)
             (mail-msg (mapconcat
                        (lambda (chg)
                          (org-with-point-at chg
                            (let ((org-trust-scanner-tags t))
                              (format "%s\n\t%s\n"
                                      (substring-no-properties
                                       (org-get-heading t t t t))
                                      (org-web-track-current-changes nil "%p => %c" " | ")))))
                        (org-web-track-update-files))))
        (unless (string-blank-p mail-msg)
          ;; SMTP settings are required in advance (see smtpmail-xxx vaiables)
          (message-mail user-mail-address "Web Tracking Notification")
          (message-goto-body)
          (insert mail-msg)
          (message-send-and-exit))))
    
    (require 'midnight)
    (add-hook 'midnight-hook #'exp/email-updated)
    (midnight-mode 1)


# Q&A


## Network Certificate Issue

Non-interactive invocation for `org-web-track-update-entry` may fail due to an unverified network certificate. This issue can occur when accessing a website that offers an unverified certificate, and the variable `network-security-level` is set to &rsquo;medium&rsquo; or higher. To address the issue, accept the certificate by calling the `org-web-track-update-entry` command interactively up-front.


# License

GPLv3

