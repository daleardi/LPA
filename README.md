LPA
===

This script calculates the Total Cost of a loan for use with APR calculations.

# Enail 1: Assignment Description

Hi Dom,

Part of this is going to be vague to see how you handle filling in the details for yourself.  From here on out consider all of this as a test of your problem solving ability.  As I mentioned before, and is common in Mathematics, focus on how you arrived at your conclusion and not solely on the answer itself.  Knowing how you arrived at your conclusion is far more valuable than seeing the answer itself...aka show your work.  ;)

You're going to use the Piecewise Function (http://www.wolframalpha.com/input/?i=piecewise+function) to solve for a total loan price.  Assume you have 3 variables being passed into your function: f(x, y, z) where x = loan amount, y = appraised value, z = FICO.  For simplicity let's restrict some aspects of the loan in question to: 30yr Conforming Fixed (so on the attached spreadsheet under the Conventional Pricing Tab we're only interested in the 30-26 Conforming & Homepath Fixed Terms table).  Obviously in a more complicated implementation we'd need that level of detail passed into our calculation, but I'm not looking to have you re-implement all of what Price Is Right already does.  :)

Once given x, y, z (from above) you will return the full loan amount (i.e. loan amount + points + LPAs) for each Rate that's listed in the table (as mentioned above).  There's an additional tab called Conventional & Govt LPAs that lists out a series of Loan Level Price Adjustments.  Let's focus on 2 LPAs (not all of them): LTV Range (for DU only), and LTV vs FICO (DU only).  I'm trying to focus this so that it's easily implementable without a series of Rules (which PiR implements already) and if checks. 

Remember 2 things: 1) This is supposed to be fun :) and 2) I realize you're not a full time software engineer.  I'm more interested in the Math (I love capitalizing Math...you'll have to deal w/ that) and your problem solving ability than I am the final solution.  If you have questions or if any of this has been unclear feel free to reach out and ask!  Text is faster than email.

John

# Email 2: First Draft 

Hey John, 

Well I'm pretty much finished with my "first draft". I put in a lot of comments about my thought process and other possible solutions. So be sure to read them all. If you have any comments, suggestions, or revisions please let me know. 

Since I know you are looking for efficiency in the script, most of my decisions were based on optimizing efficiency instead of robustness, usability, abstraction, or scalability. Also, keep in mind that my experience with R has mostly been with functions for analyzing data. The majority of the functions I used in the script are new to me and where found by searching for concepts I know from C++ and appending 'in R' to the end. I only point this out to show that I can learn on my own and adapt to challenges. 

To load the script open it in R studio and press Ctrl+Alt+R. Follow the prompts for input and watch the output. 

Thanks again, 
Dominick
