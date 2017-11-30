## Resubmission

This is a fourth resubmission. The last three reviewer comments have all concerned a single line in the description file. The most recent reads as follows:

"Thanks, please write the refenreces in the form:

Brown & Levinson, 1987 <http://psycnet.apa.org/record/1987-97641-000>; 
Danescu-Niculescu-Mizil et al., 2013 <arXiv:1306.6078>; Voigt et al., 
2017 <doi:10.1073/pnas.1702413114>"

We appreciate the help and have pasted these lines exctly into our description file. We hope this is enough to satisfy the reviewers. 

## Resubmission

This is a third resubmission. Our second resubmission received the following comment:

"The Description field contains
   <[http://psycnet.apa.org/record/1987-97641-000]Politeness: Some
Please enclose URLs in angle brackets (<...>).

Please also add a space ater the final > of the URL.
"

We have fixed the description file - there does not seem to be any clean way to include these links in the description, and since this previous work is referenced extensively in the rest of the package we have decided to simplify our description file. Our work draws on many existing methods, so it would be misleading to imply that our package is a vanilla implementation of any pre-existing work. Please let us know if there are any remaining issues. 

## Resubmission

This is a second resubmission. Our first resubmission received three related comments:

1. Unknown, possibly mis-spelled, fields in DESCRIPTION:
   'References' 'Support'
2. Please only use standard fields if possible.

3. Please move the references into the Description field.

We had followed Hadley Wickham's page on R packages, which suggests that users can write new DESCRIPTION fields. These elements would both be very useful to explain to users in separate fields, rather than jam them into the "description" field (where the formatting does not allow line breaks, thus making the file *less* readable than our approach of creating new fields). We would very much prefer to use the original description file. However, in an effort to be compliant, we have adjusted our description to match the CRAN reviewer's latest suggestion. If you are confident that our approach is an error, rather than an improvement, then we will be happy to follow your lead. As such, you can accept whichever version of the description file you prefer.

Our original submission received six comments, and our replies are in-line below.

1. please elaborate what method is used for the detection. Can 
you provide a reference in the 'Description' field of your DESCRIPTION 
file? 

This is an excellent suggestion. We had included some references in the individual functions but it makes a lot of sense to have them in the description file, as well. We are not replicating a method exactly, but combining elements from a few different papers, so we have decided to cite all of these source materials.

2. Which languages are supported?

Our package only analyzes English at the moment. But it is a good idea to make this explicit, and we have amended the description accordingly.

3. ‘Allows users researchers...' -> users or researchers

This was a typo - fixed! Thank you!

4. Please omit the blank line between YEAR and COPYRIGHT HOLDER in your file LICENSE.

Another typo - fixed! Thank you!

5. Please add more small executable examples and please do not comment out examples.

We have revised the commented-out examples to instead be tagged with \dontrun{}, as you recommended. We have also added a few more short examples to the rest of the package, documenting other use cases of the functions. However, none of our functions are that complex, and don't have a lot of user options. So we may not need as many examples as other packages. If there are particular examples that you recommend we add, then we are happy to follow your suggestions.

6. Please always write TRUE and FALSE instead of T and F in your code.

We have gone through every script and replaced all of the T/F with TRUE/FALSE. 

7. Other Revisions: 

We added a short note to the description file to reflect our funding sources, as well. 

## Test environments
* local OS X install, 3.3.2
* win-builder development and release

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs. 

  
## Downstream dependencies
No downstream dependencies