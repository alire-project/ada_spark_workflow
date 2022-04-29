# Ada SPARK Workflow
A demo of best practices for open-source Ada/SPARK development

[![codecov](https://codecov.io/gh/alire-project/ada_spark_workflow/branch/main/graph/badge.svg?token=9PZQ67LBPF)](https://codecov.io/gh/alire-project/ada_spark_workflow)

The goal of this repository is to show the best practices for Ada/SPARK open-source development. We will do our best to keep it updated with the latest technologies and services from Alire and GitHub. Don’t hesitate to suggest enhancements in case we missed something interesting.

In this project we use:

 - [`Alire`](https://alire.ada.dev) for dependency management and release publication
 - [`SPARK`](https://learn.adacore.com/courses/intro-to-spark/index.html) and [`GNATprove`](https://github.com/AdaCore/spark2014) for formal verification
 - [`AUnit`](https://github.com/AdaCore/aunit) for unit-testing
 - [`GNATcoverage`](https://github.com/AdaCore/gnatcoverage) for source code coverage analysis
 - [`GitHub Actions`](https://docs.github.com/en/actions) for Continuous Integration


# Layout of the repository

This section provides a quick overview of the different parts of the projects.

## Root directory
The root directory contains the main library:

 - `alire.toml` manifest file
 - `ada_spark_workflow.gpr` GPRbuild project file (the name matches the Alire crate name)
 - `src` directory that contains the source code for the library
 - `README.md` file in markdown format displayed on the GitHub front page

## `tests` directory

This sub-directory contains the test-suite for the library. It is an Alire project itself, with a local dependency to the main crate in the root directory, as well as other dependencies that are only required for testing (e.g. `aunit` and `gnatcov`). In this directory we also find an `alire.toml` manifest, a GPRbuild project file and a source directory.

## `.github` directory

The `.github` directory contains files specific to GitHub features and services. For this project we use GitHub Actions for the Continuous Integration (see GitHub Actions section). But there are more to explore, like templates for opening issues and pull-requests, contributions or code-of-conduct guidelines.

## `share` directory

This sub-directory contains data files to be installed with the projects.
See "Resources" section.

# How to make a similar project?

This sections provides a list of steps to create a library project with a similar workflow:

## Create a repository on GitHub

For this step you should follow [the instruction from GitHub](https://docs.github.com/en/get-started/quickstart/create-a-repo).

We recommend to use a repository name that matches the Alire crate name.
For the example below we will use the name: `my_crate`.

## Setup the layout

For this step we are going to list a few commands to run on the console.
Please note that we first export variables for the name of the Alire crate a GitHub user login to make the commands more readable.

If you want to make an application project instead of a library, you just have to replace `--lib` by `--bin` in the first `alr init` command.

```console
$ export CRATE_NAME=my_crate
$ export GITHUB_USER_NAME=mygithub
$ git clone https://github.com/${GITHUB_USER_NAME}/${CRATE_NAME}
$ cd ${CRATE_NAME}
$ alr init –in-place –lib ${CRATE_NAME}
$ mkdir tests
$ cd tests
$ alr init –in-place -bin tests
$ alr with ${CRATE_NAME} –use=..
$ alr with aunit
$ alr with gnatcov
```

## Add Continuous Integration

For this step we just create a folder for the GitHub Action workflow files and copy the workflow from this repository:
```console
$ mkdir -p .github/workflow/
$ curl https://raw.githubusercontent.com/alire-project/ada_spark_workflow/main/.github/workflows/main.yml  > .github/workflows/main.yml
```

## Enable Code Coverage reports

To see the code coverage reports in GitHub pull-requests and have a dedicated page for coverage status, you have to setup the repository in codecovio.

- First create and account and log-in: https://about.codecov.io/
- Click on "Not Yet Setup" (#1 below) to show the list of repositories available.
- Click on "setup repo" (#2 below) to enable codecovio on the new repository
- If the GitHub repository is public, you don't have to do anything more

![](https://files.readme.io/6438f9d-Screen_Shot_2022-04-13_at_9.28.09_AM.png)

Don't forget to add a [coverage status badge](https://docs.codecov.com/docs/status-badges) to your README.md file.

# Continuous Integration (GitHub Actions)

For this project we use GitHub Actions for Continuous Integration (CI).
It provides virtual machines to build and tests the project when new code is added, as well as automated validation of contributions through pull-request checks, or code coverage report with codecovio.

The only thing needed for CI to work is a "workflow" file in the `.github/workflows/` directory.

We commented as much as possible the workflow file of this repository so if you want to know more about how it works please have a look [here](.github/workflows/main.yml).

# Resources

Some projects requires non-code data files to be provided alongside the library or application executable. It can be images, databases, templates, etc. This is what we call "resources" in the Alire vocabulary.

For this demo project, we have a text file that contains a list of words, a dictionary.

Resources should be placed in the `share/<CRATE_NAME>/` directory of the repository . In addition, the following lines in the GPRbuild project file will make sure resources are installed with the library/application:
```ada
   package Install is
      for Artifacts (".") use ("share");
   end Install;
```

To be able to read the resource files, one needs to get a path to the resource folder. However the location of the resources will not always be the same depending on installation path for instance. The Alire crate `resources` provides a easy way to solve this problem. Here's how to use it:

 1. Add `resources` in the dependencies of you crate:
   ```console
   $ alr with resources
   ```
 2. Instantiate the `Resources` generic package in your code. For examples:
   ```ada
   with Resources;
   with Ada_Spark_Workflow_Config;

   package Ada_SPARK_Workflow.Resources
   is new Standard.Resources (Ada_Spark_Workflow_Config.Crate_Name);
   ```
   Note that you should replace `Ada_Spark_Workflow` with you crate name here.

 3. Get path to resources using this new package:
   ```ada
      Path : constant String := Ada_SPARK_Workflow.Resources.Resource_Path & "/unixdict.txt";

   ```

# Using Alire
# Build switches
# SPARK proof
# Unit testing
# Code Coverage
# Release
