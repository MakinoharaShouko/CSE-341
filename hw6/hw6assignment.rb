# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  # class array holding all the pieces and their rotations
  # add new pieces to the original ones
  All_My_Pieces =  All_Pieces + [rotations([[0, 0], [1, 0], [0, 1], [1, 1], [-1, 0]]), # square with extra block
                                 rotations([[0, 0], [0, 1], [-1, 0]]), # small L
                                 [[[0, 0], [1, 0], [-1, 0], [2, 0], [-2, 0]],
                                  [[0, 0], [0, 1], [0, -1], [0, 2], [0, -2]]]] # extra long (only needs two)
  # your enhancements here

  # class method to choose the next piece
  
  # get the next piece from the new pieces
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  # cheat piece
  def self.cheat (board)
    MyPiece.new([[[0, 0]]], board)
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @cheat = false  # new field to check whether the user uses cheat function or not
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    # change the boundary of the loop because
    # now there are pieces with size not equal to 4
    (0..locations.size - 1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
  
  # gets the next piece
  # if the user cheats, gives cheat piece
  # otherwise gives one of the pieces from MyPiece
  def next_piece
    if @cheat
      @current_block = MyPiece.cheat(self)
      @cheat = false  # reset cheat flag after cheating
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  # uses 100 points to cheat
  # if the cheat flag is on
  # does not consume point
  def cheat
    if (@score >= 100 and !@cheat)
      @score -= 100
      @cheat = true
    end
  end

  # rotates the current piece by 180 degrees
  def flip
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end
end

class MyTetris < Tetris
  # your enhancements here
  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)  # uses the enhanced board for the game
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  # binds two new keyboard operations
  # press u for rotating the piece 180 degrees
  # press c to cheat
  def key_bindings
    super
    @root.bind('u', proc {@board.flip})
    @root.bind('c', proc {@board.cheat})
  end
end

class MyPieceChallenge < MyPiece
  def self.next_piece (board)
    # generate the next challenge piece
    MyPieceChallenge.new(All_My_Pieces.sample, board)
  end

  # change the color of the piece
  def new_color
    @color = All_Colors.sample
  end
end

class MyBoardChallenge < MyBoard
  def initialize (game)
    @cheat = false
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    # initialize with the challenge piece
    @current_block = MyPieceChallenge.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  # increases the falling speed of the block by moving it down
  def speed_up
   if !game_over? and @game.is_running?
      @current_block.move(0, 1, 0)
   end
   draw
  end

  # changes the color of the current block
  # (pretty useless feature for this game)
  def change_color
    if !game_over? and @game.is_running?
      @current_block.new_color
    end
    draw
  end

  # gets the next piece
  # if the user cheats, gives cheat piece
  # otherwise gives one of the pieces from MyChallengePiece
  def next_piece
    if (@cheat)
      @current_block = MyPieceChallenge.cheat(self)
      @cheat = false  # reset cheat flag after cheating
    else
      @current_block = MyPieceChallenge.next_piece(self)
    end
    @current_pos = nil
  end
end

class MyTetrisChallenge < MyTetris
  def set_board
    @canvas = TetrisCanvas.new
    # sets the board to challenge board
    @board = MyBoardChallenge.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    # press i for increase falling
    @root.bind('i', proc {@board.speed_up})
    # press x to change the color of the current piece
    @root.bind('x', proc {@board.change_color})
  end
end

# To help each game of Tetris be unique.
srand
