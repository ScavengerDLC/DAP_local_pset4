### Loads library "random" in r would be library(random)
import random

### Defines a list of 15 words with each entry being a unique letter in the 
### alphabet starting at A. 
mylist = [
    "apple", "brave", "crane", "drive", "eagle",
    "fable", "grape", "house", "input", "joker",
    "knife", "lemon", "mango", "nerve", "ocean"
]
### Defines function "myfunction1" with input variables "mylist" and "mynumber2"
### we'll see how those variables come into play later
def myfunction1(mylist, mynumber2):
### Randomly selects a word from mylist and makes it lowercase
    mystring1 = random.choice(mylist).lower()
### Sets "mynumber1" equal to 0
    mynumber1 = 0

### Prints message shown in "" mynumber2 attempts seems to be the number
### of attempts for a guessing game. 
    print("Welcome!")
    print(f"You have {mynumber2} attempts.")
    
### Code will run while the max number of attempts remaining is greater than mynumber1
    while mynumber1 < mynumber2:
        
### Accepts input from console and converts that word to lowercase and removes
### extra spaces from the text
        mystring2 = input("Enter your guess: ").lower().strip()
        
### I think this checks to see if the length of your input is 5 letters (not numbers)
### Gives you an error if it isn't.
        if len(mystring2) != 5 or not mystring2.isalpha():
            print("Invalid input.")
            continue

### Adds one to the counter after the guess has been made
        mynumber1 = mynumber1 + 1

### If the string you entered matches the randomly generated one, print the message
        if mystring2 == mystring1:
            print(f"Congratulations! You won in {mynumber1} attempts.")
            break
### If the string you entered doesn't match the randomly generated one, 
### show what letters match and print remaing attempts
        else:
            mymessage = ''
            for i in range(5):
                if mystring2[i] == mystring1[i]:
                    mymessage = mymessage + mystring1[i]
                else:
                    mymessage = mymessage + '_'
            mynumber3 = mynumber2 - mynumber1
            print(f"Wrong! Here's what you got right: {mymessage}")  
            print(f"You have {mynumber3} attempts left.")

### If "mynumber1" or attempt counter reaches max attempts or "mynumber2" 
### end game and print correct word
    if mynumber1 == mynumber2:
        print(f"Sorry, you lost. The correct answer was: '{mystring1}'.")

### Starts the program with 5 attempts
myfunction1(mylist, 5)

### I lost :( 