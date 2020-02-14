*> Copyright 2017 The Advanced Terminal Processor Authors. All rights reserved.
*> Use of this source code is governed by a BSD-style
*> license that can be found in the LICENSE file.

*> cobc -x -free -fintrinsics=ALL animal.cob libhexencode.c
*>
*> NOTE: libhexencode.s contains functions to convert hex to ascii and ascii to hex.
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
 1 asciistring     Pic X(2048) value spaces.
 1 browser-input   Pic X(4096) value spaces.
 1 Args            Pic X(256) value spaces.
 1 asciilen        Pic 9(9) Comp Value 0.
 1 hexaddr usage is pointer.
 1 asciiaddr usage is pointer.
 1 hexstring       Pic X(4096) value spaces.
Linkage Section.
*> 1 testing pic 999.
procedure division.
   Initialize browser-input.
   Accept Args from command-line.
   Display trim(Args).
   
   Perform until trim(browser-input) = "exit"
   *>
   *> NOTE:
   *> All browser-input will be hexidecimal (base16)
   *> After 'accept' read into browser-input, convert from hex to ascii.
   *>
      Move Spaces to browser-input
      Accept browser-input
      Compute asciilen = length(Trim(browser-input)) / 2
      Move Spaces to hexstring asciistring
      String Trim(browser-input) x"00" Delimited by size into hexstring End-String
      Call "HEX2ASCII" Using by reference hexstring, by reference asciistring
      Move Spaces To browser-input      
      Move asciistring(1:asciilen) to browser-input
      Evaluate trim(browser-input)
          When "cat"
              Move Spaces To asciistring hexstring
              String "The domestic <span style=" quote "color:red; font-size:32px;" quote ">cat</span> is a small, typically furry, "
                "carnivorous mammal. They are often called house "
                "cats when kept as indoor pets or simply cats "
                "when there is no need to distinguish them from "
                "other felids and felines." Delimited by size into asciistring
              End-String
              display trim(asciistring)
          When "dog"
              Move Spaces To asciistring hexstring
              String "The domestic <b>dog</b> is a member of genus Canis that"
                " forms part of the wolf-like canids, and is the"
                " most widely abundant carnivore. "
                " Dogs are: Canis lupus familiaris" Delimited by size into asciistring
              End-String
              display trim(asciistring)
          When other
                If trim(browser-input) <> "exit"
                    Move Spaces To asciistring hexstring
                    String "<h1>I only like dogs and cats.</h1>"
                      Delimited by size into asciistring
                    Display Trim(asciistring) upon SYSERR
                End-If
      End-Evaluate
   End-Perform.
   Display "exiting"
   Stop Run Returning 0.
