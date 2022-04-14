Verification and Validation Plan
================================

The goals of this plan is to document all steps to release the product "genc³" as an online-product aiming to satisfiy the endusers expectations regarding download, installation, use, and further information.

Test Rules
----------

- All tests are automated by using the bash file "st".
	- The test results are written into file "TestResult.txt" in folder "test".
- Checks are conducted before dry runs of test:
	- All functions, classes, laws of classes (if applicable) are defined by code comments
	- All functions have test tests in corresponding file "Spec___.hs"
		- e.g. functions in file "./src/CharCode.hs" have tests in "./test/SpecCharCode.hs"
	- All tests are complete
		- test all functions at edge cases, and boundaries
		- test all function properties (probabilistically)
		- test all characteristics at edge cases, and boundaries
		- test all characteristics as properties (probabilistically)
- Checks are conducted after test:
	- File "testresult.txt" in folder "test" is newer than all products and by-products, at least including:
		- executables
		- libraries
		- documentation
	- All functions in libraries have a corresponding file "Spec___.hs"
		- e.g. for the file "./src/CharCode.hs" is also a file "./test/SpecCharCode.hs"
	- All spec modules are imported and used in file "./test/Spec.hs", by 
		- review of Spec.hs-files ("./test/Spec.hs", "./test/ModuleSpec.hs")
		- review of "TestResult.txt" in folder "test"
			- criteria:
				- tests for all classes, it's laws and it's functions that are validated according to Spec.hs-files are also executed
	- All functions in all modue "Spec___.hs"
		- e.g. for the file "./src/CharCode.hs" is also a file "./test/SpecCharCode.hs"
	- All tests are OK
	- All the check results are documented in a Software Configuration File, including
		- Software Name
		- Software Version Number
		- failed tests and/or deviations
		- timestamps
		- filenames
		- module names
		- file checksums

Pre-Release Rules
-----------------

- The copying of the software product and by-products to an online pre-release folder is executed by the bash file "spr".
- A final end-test is executed to test
	- Download procedures
	- Installation procedures
	- Opening of documentation
	- Function of the product
	- Availability of licensing information
	- Availability of disclaimers

Release Rules
-------------

- After successful final end test according to the pre-release rules, 
	- the copying of the software product and by-products to an online release folder is executed by the bash file "sr".
