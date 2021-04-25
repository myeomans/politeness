## Version Update (0.7.8)

Update for new quanteda version


## Version Update (0.7.7)

Smoother negation handling

## Version Update (0.7.6)

removed dependency on textclean as it was throwing strange warnings. instead, we have a local function to convert to ascii, and borrow from stringi as well. 

## Version Update (0.7.5)

Making it easier to extract coefficients of model without running full cross-validation in politenessProjection

Fixing calculation for new CI feature in politenessPlot

Add new feature to main engine - subjectivity

remove redundant features from the hedges list

updated the vignette throughout

added a conversion tool for UK english spellings

## Version Update (0.7.0)

Larger changes - we have added a nested cross-validation option to the  politenessProjection function. We have also changed default behavior of the top-level function - the drop.blank argument now defaults as true, as basically all uses of the function include the full set. This new behavior does not affect any dependencies, as the drop.blank functionality was a mere convenience. This is a more sensible and common default.


## Version Update (0.6.2)

We have changed some of the features to accommodate British spelling.

## Version Update (0.6.1)

We have adjusted the standard errors in the plot function to be customizable (e.g. for 95% CIs) and fixed a bug in the original formula.

## Version Update (0.6.0)

Based on user feedback, We re-wrote the code to handle large corpora better. When more than 2000 documents are included, we automatically batch the results in a loop, with a progress bar. We also removed a long-since deprecated argument (binary) from the main function.

## Version Update (0.5.2)

Based on user feedback, we re-wrote the pre-trained model to be fully pre-trained, rather than re-training each time on identical data. This will make the function much faster, and also eliminate any randomness due to algorithm initialization.

## Version Update (0.5.1)

Re-naming some of the outputs in the main function broke one of our pre-trained models, which had been trained using different labels. So we re-trained the pre-trained model. We tried to write a new set of tests for the pre-trained model so that this kind of error is caught in future, but they don't seem to work on the CRAN server (only locally) so the test are commented out for now.

## Version Update (0.5.0)

New package (textclean) used to clean sanitize character inputs. 
New email address for maintainer (got a new job! so old email will expire soon)
Updated advice for spacy installation

## Version Update (0.4.4)

Some revisions to documentation. Also adjsuted to account for changes to glmnet functions


## Version Update (0.4.1)

Correcting some typos in the last version (sorry).

## Version Update (0.4.0)

Added a new function with a pre-trained model from a related paper.

## Version Update (0.3.3)

Based on some user testing we have clarified some common error messages. We also have added more documentation regarding the plot options. 

## Version Update (0.3.2)

Some of the new features were redundant with older ones, so we removed three early features that are now redundant. 

## Version Update (0.3.1)

We have updated three of the included features (question types and affirmation), and added two new ones (agreement, acknowledgement). We have also updated the documentation to reflect these changes, as well as changes in one of our dependencies (spacyr).

## Version Update (0.2.6)

Sadly, the labels we updated in the previous version disrupted some functionality (which we discovered from a collaborator soon after the previous push). We've gone in and fixed the issue, and we apologize for the error.

## Version Update (0.2.5)

We have added several small updates, in particular clarifying the labels on some of our features, and updating our vignette based on improvements in one of our package dependencies (SpaCyR).

## Version Update (0.2.4)

We have added several small updates. We have changed some of the arguments in the main function (maintaining backward compatibility with the previous "binary" argument). We have also updated some of the documentation, and revised the definition of "gratitude", one of the features extracted in the main function.

## Version Update (0.2.2)

This is a small update, to reflect some concerns relayed to us by the CRAN team. One of our dependencies was failing a few of our tests, so we removed it as a default (relying on a more stable package instead), and dropped the problematic tests.

## Version Update (0.2.1)

This is our second version update. In addition to some minor typo corrections,
we have made two substantial changes:

1. Reworked the part-of-speech tagging functions to use data.table, and replaced a superfluous subordinate function. These steps greatly improve the speed of execution, especially for larger datasets.

2. Added a short discussion (with figures) on expected execution time to the vignette, so that users are not flummoxed by long run times.

## Resubmission

This is a resubmission of our first update below. We mistakenly did not advance the version number, so now we have incremented the version number.

## Version Update

This is our first version update. Based on initial user reports, we have made the following modifications:

1. We have added support for a continuous outcome in the main politeness functions.

2. We have corrected a bug in the calculation of bare commands.

3. We have modified some of the documentation to accomodate feedback.

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
