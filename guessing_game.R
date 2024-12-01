library(tidyverse)
### Creates word bank
word_bank <- c("apple", "brave", "crane", "drive", "eagle",
               "fable", "grape", "house", "input", "joker",
               "knife", "lemon", "mango", "nerve", "ocean")

### Creates Function
guessing_game <- function(word_bank, max_guesses){
  ### Roll Number
  number <- sample.int(15, 1)
  ### Use randomly generated number to select word from word bank
  secret_word <-  {{word_bank}}[number]
  
  ### Perform Transformations done in Python Program
  secret_word <- str_to_lower(secret_word)
  
  secret_word <- str_squish(secret_word)
  
  ### Creat Attempt Counter
  attempt_counter <- 0
  
  ### Welcome Message
  print("Welcome!")
  print(paste0("You have ", {{max_guesses}}, " attempts"))
  
  ### Game Logic
  while (attempt_counter < {{max_guesses}}) {
    
    ### User Input
    guess <- readline(prompt = "Enter your guess: ")
    
    ### Transformations to guess to make it in the same format as word
    guess <- str_to_lower(guess)
    
    guess <- str_squish(guess)
    
    ### Check for valid input, 5 letters (not numbers or other characters)
    if (nchar(guess) != 5 | grepl("[0-9]", guess) == TRUE | grepl("[^A-Za-z0-9 ]", guess) == TRUE) {
      print("Invalid Guess")
      next
    }
    
    ### Increase attempt counter
    attempt_counter <- attempt_counter + 1
    
    ### Win Condition
    if (guess == secret_word){
      print(paste0("Congratulations! You guessed the word in ", attempt_counter, " tries!"))
      break
    }
    
    ### Tests to see if your guess shares any letters with the correct word
    ### Placement is important
    if (guess != secret_word){
      hint <- ""
      for (i in 1:5) {
        ifelse(substr(guess, i, i) == substr(secret_word, i, i),
               hint <- paste0(hint, substr(guess, i, i)),
               hint <- paste0(hint, "_"))
      }
      
      attempts_remaining <- {{max_guesses}} - attempt_counter
      print(paste0("Wrong! Here's what you got right:", hint))
      print(paste0("You have ", attempts_remaining, " attempts remaining!"))
    }
  }
  
  ### Game Ender, if Attempt Counter equals max attempts
  if (attempt_counter == {{max_guesses}}){
    print(paste0("Game Over: The Correct word was ", secret_word))
  }
}

guessing_game(word_bank, 5)