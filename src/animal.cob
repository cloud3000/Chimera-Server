*> Copyright 2017 The Advanced Terminal Processor Authors. All rights reserved.
*> Use of this source code is governed by a BSD-style
*> license that can be found in the LICENSE file.

*> cobc -x -free -ffunctions-all animal.cob
*>
IDENTIFICATION DIVISION.
PROGRAM-ID. animal.
AUTHOR.     Michael Anderson.
DATE-COMPILED.
environment division.
configuration section.
*> special-names.
*>    SYSERR is SERR.
SOURCE-COMPUTER. GNU-Linux.
OBJECT-COMPUTER. GNU-Linux.
data division.
WORKING-STORAGE SECTION.
 1 browser-input Pic X(1024) value spaces.
 1 Args          Pic X(256) value spaces.
Linkage Section.
*> 1 testing pic 999.
procedure division.
   Initialize browser-input.
   Accept Args from command-line.
   Display trim(Args).
   Perform until trim(browser-input) = "exit"
      Accept browser-input
      Evaluate trim(browser-input)
          When "cat"
              Display "The domestic <span style=" quote "color:red; font-size:32px;" quote ">cat</span> is a small, typically furry, "
              Display "carnivorous mammal. They are often called house "
              Display "cats when kept as indoor pets or simply cats "
              Display "when there is no need to distinguish them from "
              Display "other felids and felines."
          When "dog"
              Display "The domestic <b>dog</b> is a member of genus Canis that"
              Display "forms part of the wolf-like canids, and is the "
              Display "most widely abundant carnivore. "
              Display "Dogs are: Canis lupus familiaris"
          When other
                If trim(browser-input) <> "exit"
                    Display "<h1>I only like dogs and cats.</h1>" upon SYSERR
                    *> move 1 to testing
                End-If
      End-Evaluate
   End-Perform.
*>   move 1 to testing.
   Display "exiting"
   Stop Run Returning 0.
