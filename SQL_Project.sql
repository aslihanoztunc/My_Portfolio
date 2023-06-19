/********************************************
Name: Aslihan Oztunc
Date: 11/26/2022

SQL Project
********************************************/

/********************************************
-- PART A

-- CREATE OUR DATABASE schooldb
-- We run DROP IF EXISTS statement first
********************************************/

USE Master;
GO
DROP DATABASE IF EXISTS schooldb;
GO
CREATE DATABASE schooldb;
GO

PRINT 'Part A Completed'
GO

-- ****************************
-- PART B

-- Creates the Stored Procedure usp_dropTables  
-- to remove all the tables from DB.
-- ****************************

USE schooldb;
GO

DROP PROCEDURE IF EXISTS usp_dropTables -- Drop Procedure if exists
GO

CREATE PROCEDURE usp_dropTables
AS 
BEGIN
   DROP TABLE IF EXISTS StudentContacts
   DROP TABLE IF EXISTS Student_Courses
   DROP TABLE IF EXISTS Employees
   DROP TABLE IF EXISTS EmpJobPosition
   DROP TABLE IF EXISTS ContactType
   DROP TABLE IF EXISTS CourseList
   DROP TABLE IF EXISTS StudentInformation
      
END;

-- Uncomment to execute the Stored Procedure
-- EXEC usp_dropTables 

GO

PRINT 'Part B Completed'

-- ****************************
-- PART C

-- Creates the tables from the Entity Relationship Diagram
-- Write the CREATE TABLE statements for each of the tables in the Entity Relationship Diagram.
-- Integrates the PRIMARY KEY and FOREIGN KEY CONSTRAINTS in the CREATE TABLE statement.
-- ****************************

GO 

CREATE TABLE StudentInformation
(
   StudentID INT NOT NULL
      IDENTITY(100,1) PRIMARY KEY, -- PRIMARY KEY starts at 100, increments 1
   Title NVARCHAR(20) NULL,
   FirstName NVARCHAR(100) NOT NULL,
   LastName NVARCHAR(100) NOT NULL,
   Address1 NVARCHAR(100) NULL,
   Address2 NVARCHAR(100) NULL,
   City NVARCHAR(50) NULL,
   Country NVARCHAR(50) NULL,
   Zip NVARCHAR(10) NULL,
   Telephone NVARCHAR(20) NULL,
   Email NVARCHAR(50) NULL,
   Enrolled BIT NULL,
   AltTelephone NVARCHAR(20) NULL
)

CREATE TABLE CourseList (
   CourseID INT  NOT NULL 
      IDENTITY(10,1) PRIMARY KEY, -- PRIMARY KEY starts at 10, increments 1
   CourseDescription NVARCHAR(255) NOT NULL,
   CourseCost NVARCHAR(10) NULL,
   CourseDurationYears INT NULL, 
   Notes NVARCHAR(255) NULL
)

CREATE TABLE ContactType (
   ContactTypeID INT NOT NULL 
      IDENTITY(1,1) PRIMARY KEY, -- PRIMARY KEY starts at 1, increments 1
   ContactType NVARCHAR(80) NOT NULL
)

CREATE TABLE EmpJobPosition (
   EmpJobPositionID INT NOT NULL
      IDENTITY(1,1) PRIMARY KEY, -- PRIMARY KEY starts at 1, increments 1
   EmployeePosition NVARCHAR(50) NOT NULL
)

CREATE TABLE Employees (
   EmployeeID INT NOT NULL 
      IDENTITY(1000,1) PRIMARY KEY, -- PRIMARY KEY starts at 1000, increments 1
   EmployeeName NVARCHAR(100) NOT NULL,
   EmployeePositionID INT NOT NULL 
      FOREIGN KEY REFERENCES EmpJobPosition(EmpJobPositionID), -- FOREIGN KEY with references
   EmployeePassword NVARCHAR(50) NULL,
   Access BIT NULL,
)
CREATE TABLE Student_Courses (
   StudentCourseID INT NOT NULL 
      IDENTITY(1,1) PRIMARY KEY, -- PRIMARY KEY starts at 1, increments 1
   StudentID INT NOT NULL 
      FOREIGN KEY REFERENCES StudentInformation(StudentID), -- FOREIGN KEY with references
   CourseID INT NOT NULL 
      FOREIGN KEY REFERENCES CourseList(CourseID), -- FOREIGN KEY with references
   CourseStartDate DATE NOT NULL,
   CourseCompleteDate DATE NULL
)
CREATE TABLE StudentContacts (
   ContactID INT NOT NULL
      IDENTITY(10000,1) PRIMARY KEY, -- PRIMARY KEY starts at 10000, increments 1
   StudentID INT NOT NULL
      FOREIGN KEY REFERENCES StudentInformation(StudentID), -- FOREIGN KEY with references
   ContactTypeID INT NOT NULL
      FOREIGN KEY REFERENCES ContactType(ContactTypeID), -- FOREIGN KEY with references
   ContactDate DATE NOT NULL,
   EmployeeID INT NOT NULL
      FOREIGN KEY REFERENCES Employees(EmployeeID), -- FOREIGN KEY with references
   ContactDetails NVARCHAR(255) NOT NULL
)

PRINT 'Part C Completed'

-- ****************************
-- PART D 
-- We alter our tables to add columns, constraints, and indexes

-- Adds a new column CreatedDateTime as current timestamp to the StudentInformation. 
-- Removes the AltTelephone column from the StudentInformation table
-- Adds an Index called IX_LastName on the StudentInformation table
-- ****************************

GO
-- Alter Student_Courses to prevent duplicate records 
ALTER TABLE Student_Courses WITH NOCHECK 
   ADD CONSTRAINT Student_Courses_UNIQUE UNIQUE(StudentID, CourseID)

-- Alter StudentInformation to add CreatedDateTime
ALTER TABLE StudentInformation
   ADD CreatedDateTime datetime NOT NULL DEFAULT(CURRENT_TIMESTAMP)

-- Alter StudentInformation to drop AltTelephone column
ALTER TABLE StudentInformation
   DROP COLUMN AltTelephone

CREATE INDEX IX_LastName ON StudentInformation(LastName)


PRINT 'Part D Completed'

-- ****************************
-- PART E
-- Creates the Trigger trg_assignEmail on the StudentInfomation table
-- Trigger will fire and update the Email field of the record if a new row is inserted into StudentInformation without the Email field
-- Email has pattern: firstName.lastName@disney.com
-- ****************************

GO

CREATE TRIGGER trg_assignEmail ON StudentInformation
AFTER INSERT
AS

-- Declare Variables
DECLARE 
   @StudentID INT,
   @FirstName NVARCHAR(100),
   @LastName NVARCHAR(100)

BEGIN
   SELECT
      @StudentID = inserted.StudentID,
      @FirstName = inserted.FirstName,
      @LastName = inserted.LastName 
   FROM inserted

-- Update Email field with the concatenation of FirstName, LastName and pattern domain
   UPDATE StudentInformation
      SET Email = @FirstName + '.' + @LastName + '@disney.com'
      WHERE StudentID = @StudentID AND Email IS NULL
END

GO

--Uncomment to test the trigger trg_assignEmail 
/*
INSERT INTO StudentInformation
   (FirstName,LastName,Email)
VALUES
   ('Porky', 'Pig', 'porky.pig@warnerbros.com');

INSERT INTO StudentInformation
   (FirstName,LastName)
VALUES
   ('Snow', 'White');

GO
*/

PRINT 'Part E Completed'

-- ****************************
-- Part F
-- DATA Population
-- ****************************

GO

INSERT INTO StudentInformation
   (FirstName,LastName)
VALUES
   ('Mickey', 'Mouse');

INSERT INTO StudentInformation
   (FirstName,LastName)
VALUES
   ('Minnie', 'Mouse');

INSERT INTO StudentInformation
   (FirstName,LastName)
VALUES
   ('Donald', 'Duck');
SELECT * FROM StudentInformation;

INSERT INTO CourseList
   (CourseDescription)
VALUES
   ('Advanced Math');

INSERT INTO CourseList
   (CourseDescription)
VALUES
   ('Intermediate Math');

INSERT INTO CourseList
   (CourseDescription)
VALUES
   ('Beginning Computer Science');

INSERT INTO CourseList
   (CourseDescription)
VALUES
   ('Advanced Computer Science');
select * from CourseList;

INSERT INTO Student_Courses
   (StudentID,CourseID,CourseStartDate)
VALUES
   (100, 10, '01/05/2018');

INSERT INTO Student_Courses
   (StudentID,CourseID,CourseStartDate)
VALUES
   (101, 11, '01/05/2018');

INSERT INTO Student_Courses
   (StudentID,CourseID,CourseStartDate)
VALUES
   (102, 11, '01/05/2018');
INSERT INTO Student_Courses
   (StudentID,CourseID,CourseStartDate)
VALUES
   (100, 11, '01/05/2018');

INSERT INTO Student_Courses
   (StudentID,CourseID,CourseStartDate)
VALUES
   (102, 13, '01/05/2018');
select * from Student_Courses;

INSERT INTO EmpJobPosition
   (EmployeePosition)
VALUES
   ('Math Instructor');

INSERT INTO EmpJobPosition
   (EmployeePosition)
VALUES
   ('Computer Science');
select * from EmpJobPosition

INSERT INTO Employees
   (EmployeeName,EmployeePositionID)
VALUES
   ('Walt Disney', 1);

INSERT INTO Employees
   (EmployeeName,EmployeePositionID)
VALUES
   ('John Lasseter', 2);

INSERT INTO Employees
   (EmployeeName,EmployeePositionID)
VALUES
   ('Danny Hillis', 2);
select * from Employees;

INSERT INTO ContactType
   (ContactType)
VALUES
   ('Tutor');

INSERT INTO ContactType
   (ContactType)
VALUES
   ('Homework Support');

INSERT INTO ContactType
   (ContactType)
VALUES
   ('Conference');
SELECT * from ContactType;

INSERT INTO StudentContacts
   (StudentID,ContactTypeID,EmployeeID,ContactDate,ContactDetails)
VALUES
   (100, 1, 1000, '11/15/2017', 'Micky and Walt Math Tutoring');

INSERT INTO StudentContacts
   (StudentID,ContactTypeID,EmployeeID,ContactDate,ContactDetails)
VALUES
   (101, 2, 1001, '11/18/2017', 'Minnie and John Homework support');

INSERT INTO StudentContacts
   (StudentID,ContactTypeID,EmployeeID,ContactDate,ContactDetails)
VALUES
   (100, 3, 1001, '11/18/2017', 'Micky and Walt Conference');

INSERT INTO StudentContacts
   (StudentID,ContactTypeID,EmployeeID,ContactDate,ContactDetails)
VALUES
   (102, 2, 1002, '11/20/2017', 'Donald and Danny Homework support');

SELECT * from StudentContacts;

-- Note for Part E, use these two inserts as examples to test the trigger
-- They will also be needed if you’re using the examples for Part G
INSERT INTO StudentInformation
   (FirstName,LastName,Email)
VALUES
   ('Porky', 'Pig', 'porky.pig@warnerbros.com');
INSERT INTO StudentInformation
   (FirstName,LastName)
VALUES
   ('Snow', 'White');

/* Remove comment when Part B and Part C are completed */

PRINT 'Part F Completed'

-- ****************************
-- PART G
-- Creates a stored procedure that allows for quick adds of Student and Instructor contacts: usp_addQuickContacts.
-- The procedure will accept 4 parameters:  Student Email, EmployeeName, contactDetails, contactType 
-- And performs an INSERT into the StudentsContacts table.
-- When inserting into StudentsContacts, the ContactDate field will automatically default to the current Date.
-- If the contactType parameter value doesn’t exist in the ContactType table, it's first inserted as a new record AND then used with an INSERT statement to the StudentContacts.
-- If the contactType parameter value does already exist, it's corresponding ID is added as part of the StudentContacts INSERT statement.
-- ****************************

GO

CREATE PROCEDURE usp_addQuickContacts
   @StudentEmail NVARCHAR(50), -- Parameter declarations
   @EmployeeName NVARCHAR(100),
   @contactDetails NVARCHAR(255),
   @contactType NVARCHAR(80)
AS

DECLARE
   @student_id INT, -- Variable declarations
   @contact_type_id INT,
   @contact_date DATE = GETDATE(),
   @employee_id INT,
   @contact_details NVARCHAR(255) = @contactDetails;

-- Get the student ID
   SELECT 
      @student_id = StudentID
   FROM 
      StudentInformation
   WHERE 
      Email = @StudentEmail;

-- Get the employee ID
   SELECT
      @employee_id = EmployeeID
   FROM
      Employees
   WHERE
      EmployeeName = @EmployeeName;


-- IF ContactType exists in the ContactType table retrieve ContactTypeID, 
-- ELSE insert new ContactType and return ContactTypeID

IF EXISTS (
   SELECT * FROM ContactType 
   WHERE ContactType = @contactType
   )

   SELECT 
      @contact_type_id = ContactTypeID -- Get ContactTypeID if ContactType exists
   FROM 
      ContactType
   WHERE 
      ContactType = @contactType

ELSE
   BEGIN
      INSERT INTO 
         ContactType (ContactType) 
      VALUES (@contactType);
      SET 
         @contact_type_id = SCOPE_IDENTITY(); -- Returns the last identity value inserted
   END

INSERT INTO 
   StudentContacts(
      StudentID,
      ContactTypeID,
      ContactDate,
      EmployeeID,
      ContactDetails
      ) 
   VALUES (
      @student_id,
      @contact_type_id,
      @contact_date,
      @employee_id,
      @contact_details
   );

GO

--Uncomment to test the output
/*
EXEC usp_addQuickContacts 'minnie.mouse@disney.com','John Lasseter','Minnie getting Homework Support from John','Homework Support' 
EXEC usp_addQuickContacts 'porky.pig@warnerbros.com','John Lasseter','Porky studying with John for Test prep','Test Prep'

GO
*/

PRINT 'Part G Completed'

-- ****************************
-- PART H

-- Creates a stored procedure: usp_getCourseRosterByName.
-- It takes 1 parameter, CourseDescription and returns the list of the student’s FirstName, and LastName along with the CourseDescription they are enrolled in.  
-- Note:  Use JOINS.
-- ****************************

GO

CREATE PROCEDURE usp_getCourseRosterByName 
   @CourseDescription NVARCHAR(255) -- Parameter declarations
AS
   SELECT 
      CourseList.CourseDescription,
      StudentInformation.FirstName,
      StudentInformation.LastName 
   FROM CourseList
   INNER JOIN 
      Student_Courses ON CourseList.CourseID = Student_Courses.CourseID
   INNER JOIN 
      StudentInformation ON Student_Courses.StudentID = StudentInformation.StudentID      
   WHERE
      CourseList.CourseDescription = @CourseDescription

GO

-- Uncomment to execute the Stored Procedure. 
/*
   EXEC usp_getCourseRosterByName 'Intermediate Math';
   GO
*/

PRINT 'Part H Completed'

-- ****************************
-- Part I
-- Create a view : vtutorContacts, 
-- Returns the results from StudentContacts displaying fields EmployeeName, StudentName, ContactDetails, and ContactDate where the contactType is ‘Tutor’. 
-- EmployeeName doesn’t exist in StudentContacts, and may require a JOIN from another table.
-- StudentName doesn't exist, but should be in the form FirstName+' '+LastName. Ensuring both First and Last name are properly trimmed.
-- ****************************

GO

CREATE VIEW vtutorContacts 

AS
   SELECT 
      Employees.EmployeeName,
      CONCAT(TRIM(StudentInformation.FirstName), ' ', TRIM(StudentInformation.LastName)) AS StudentName,  -- Concat Student Name
      StudentContacts.ContactDetails,
      StudentContacts.ContactDate
   FROM
      StudentContacts
   INNER JOIN
      Employees ON Employees.EmployeeID = StudentContacts.EmployeeID
   INNER JOIN
      StudentInformation ON StudentInformation.StudentID = StudentContacts.StudentID
   INNER JOIN
      ContactType ON ContactType.ContactTypeID = ContactType.ContactTypeID
   WHERE
      ContactType.ContactType = 'Tutor';

-- To test View
-- SELECT * FROM vtutorContacts
GO

PRINT 'Part I Completed'

GO


