# Explanation: Formatting

The whole purpose of this program existing is to format PureScript source code.
We make some decisions about how to format that are not always apparent when looking at the formatted output.
To provide some information about why those decisions are made, we provide some of the goals and non-goals of this program.

## Goals

The goals for the project should help guide formatting decisions.
They should also provide some clue to someone seeing unfamiliar formatting a first time.

Some of these factors are:
* Mitigating bike-shedding.
    * Any time there are multiple people writing code on a project, they are going to have different opinions about how to format some code.
        There is a non-trivial amount of time wasted discussing how to format code and then making the changes to format code a certain way.
    * Rather than encouraging people to spend their energy discussing formats, we have one style that formats code the same way given a specific input.
* Provide a bit of control over formatting.
    * Worse than bike-shedding is creating animosity within a team.
        If a formatting tool is too opinionated, people will not use it.
        We end up walking a fine line of letting people format things how they want, and minimizing the amount of wasteful bike-shedding people do when creating a program.
    * To foster adoption, we don't want to squander all creativity.
    * There isn't one best way to format everything.
        For example, formatting an array depends quite a bit on the elements of the array.
        An array with a few elements that are a couple of characters might make sense to have on a single line.
        An array that represents some structural layout that spans multiple lines might make sense to have on multiple lines.
* `diff`ability of the code.
    * A big part of writing code is that it evolves over time.
        The layout of the code can have a huge influence over whether the `diff` of code helps you understand the change.
    * Prioritizing signal over noise in diffs is a pretty big motivator behind this program.
        For example, let's say the formatting algorithm is to try and put as much on a single line as possible and then wrap at some arbitrary number (say 80 characters).
        If you've got some line that is 78 characters long, and you rename a value to have a few more characters, the diff will show that you deleted one line and added at least two more.
        A human has to go and read the removed/added code and perform a manual diff in their brain.
        If the formatting algorithm does not take line length into account, the diff of renaming a value can show only the bits that changed more clearly.
* Consistency of formatting.
    * Two constructs that are similar (like function application and type application) should be formatted similarly.
        For example, knowing how function applications are formatted means that you can start to notice patterns in code that help understanding better than before.
    * Two constructs that are similar (like syntactic operators and user-defined operators) but formatted differently should have an important purpose for being formatted differently.
        For example, most syntactic operators are postfix.
        Some syntactic operators are prefix.
        User-defined operators sometimes make more sense as a postfix operator and sometimes as a prefix operator.
        Since we know every syntactic operator statically, we can make a decision as to whether it should be prefix or postfix.
        Since we cannot know any user-defined operators statically, we would have a hard time determining whether to format it prefix or postfix.
        We make a single decision for all user-defined operators that is not the same as the decisions we maake for user-defined operators.

## Non-Goals

It is equally important to mention ideas that could influence a formatting decision, but that are explicitly not goals of this program.

* Familiarity with other code currently in the wild in PureScript.
    * It doesn't really matter if everyone is formatting code some way already in PureScript.
        We do not format the code in order to be familiar with what is already out in the wild.
    * Appeals to popularity are a logical fallacy for a reason.
    * There are quite a few common formatting patterns that exist primarily because of inertia.
    * Continuing to follow those patterns for no apparent benefit is not a goal of this program.
    * We don't actively choose to be different, but an appeal to popularity will not carry much (if any) weight in the formatting decision.
* Familiarity with other code in different languages.
    * PureScript is not these other languages.
    * There are some decisions made for PureScript that tend to make formatting a certain way better than others.
        For example, PureScript's constraints in type signatures are singular.
        The `=>` syntax has to be used to have more than one constraint.
        That design decision makes `=>` as important an operator as `->`, so we format type signatures differently than in elm or Haskell.
    * Most other languages have different factors behind why they are the way they are.
        Some languages have a self-appointed leader that decides things.
        Some languages are without a unifying vision.
        These people choices lead to people formatting code a certain way.
* Line length.
    * Altering format based on line length tends to produce more noise in diffs than not.
        A rename of a few characters can have large noisy changes throughout a section of code or an entire file.
    * Indentation based languages are a bit more amenable to line length changes.
        A language that is not indentation sensitive can get away with using line-based formatting much easier than PureScript.
        An indentation sensitive language sometimes cannot join lines together or split them to multiple lines.
        It depends more on the syntactic construct than the length of the line.
* Compactness or verbosity.
    * Neither one is inherently better than the other.
    * Something can be formatted compactly and be an improvement, or it could be the other way.