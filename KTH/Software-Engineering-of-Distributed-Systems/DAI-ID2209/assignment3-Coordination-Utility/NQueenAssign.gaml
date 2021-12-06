/***
* Name: NQueensAssign
* Author: Abdullah, Aksel
* Description: 
* Tags: Tag1, Tag2, TagN
***/

model NQueensAssign

/* Insert your model definition here */

global {
	
	int N <- 4;
	int boxSize <- 10;
	int width <- boxSize * N;
	geometry shape <- square(width);
	
	Queen headQueen <- nil;
	
	init {
		
		Queen lastQueen <- nil;
		int nQueens <- N;
		loop row from: 0 to: N - 1 {
			create Queen returns: q {
				N <- nQueens;
				location <- {boxSize/2, (boxSize/2) + row * boxSize}; // each box is width 10 so 5 means start from center
				predecessor <- lastQueen;		
				do moveTo(0); // start all at 0 box
			}
			
			lastQueen <- q[0];
			if row = 0 {
				headQueen <- lastQueen;
			}
		}
		
		// setting up successors
		Queen q <- lastQueen; // q[N-1];
		loop while: q.predecessor != nil {
			q.predecessor.successor <- q;
			q <- q.predecessor;
		}
		
		headQueen.isReady <- true; // first queen is always ready to start the movement
	}

}

grid ChessBoard width: N height: N {
		
	rgb white <- rgb('black');
	rgb black <- rgb('white');
	
	int position <- (grid_x + grid_y) mod 2;
	
	map<int,rgb> EvenOddColor <- [0::white, 1:: black];
	
	rgb color <- EvenOddColor[position];
}


species Queen skills: [moving, fipa] {
	Queen predecessor <- nil;
	Queen successor <- nil;
	int N <- int(nil);
	int position <- 0;
	point targetPoint <- nil;
	bool isReady <- false;
	bool canMove <- true;
	map<int,bool> visited;
	
	reflex moveToTarget when: targetPoint != nil {
		do goto target: targetPoint;
	}
	
	action moveTo(int p) {
		position <- p;
		visited[p] <- true;
		targetPoint <- {(boxSize/2) + boxSize * p, location.y};
	}
	
	// Returns true if the given position is safe from previous predecessors.
	bool isPositionSafe(int pos) {
		
		int iterate <- 1;
		Queen q <- predecessor;
		loop while: q != nil {

			if pos = q.position or (pos - iterate) = q.position or (pos + iterate) = q.position {
				return false;
			}
			q <- q.predecessor;
			iterate <- iterate + 1;
		}
		return true;
	}
	
	bool moveToNextSafePosition
	{
		loop i from: 0 to: N - 1 {
			int newPos <- mod(position + i, N);
			//write newPos;
			write name + ': Checking new position ' + newPos;
			if visited[newPos] = true {
				write name + " already visited";
			} else if isPositionSafe(newPos) {
				do moveTo(newPos);
				write name + " placed at " + position;
				return true;
			}
		}
		return false;
	}
	
	// When all predecessors are placed, try to place myself
	reflex placeMySelf when: (predecessor != nil and predecessor.isReady and !isReady and canMove) {
		write name + ': Finding a place';
		
		if (!moveToNextSafePosition()) {
			canMove <- false;
		} else {
			isReady <- true;
		}
	}

	reflex requesPredecessor when: !canMove {
				
		write name + ' can not move, requesting predecessor';
		
		if predecessor = nil {
			write "No places available";
		}
		
		do start_conversation (to ::[predecessor], protocol:: 'fipa-request', performative:: 'request', contents::[self.name, "move-next"]);
		canMove <- true;
	}
	
	reflex readRequestMessage when: !(empty(requests)) {
		write name + ' received move-next request';
		
		message r <- (requests at 0);
		
		list<unknown> c <- r.contents;
		
		string x <- string(c[1]);
		
		if x = 'move-next' {
			if (!moveToNextSafePosition()) {
				canMove <- false;
				isReady <- false;
			} else {
				do start_conversation (to ::[successor], protocol:: 'fipa-request', performative:: 'request', contents::[self.name, "reset-successor"]);
			}
		} else if x = 'reset-successor' {
			loop i from: 0 to: N {
				visited[i] <- false;
			}
			if successor != nil {
				do start_conversation (to ::[successor], protocol:: 'fipa-request', performative:: 'request', contents::[self.name, "reset-successor"]);
			}
		}
	}

	aspect base {
		color <- #green;
    	draw sphere(1) color:color;
    }
}

experiment main type:gui {
	output {
		display my_display type: opengl
		{
			species Queen aspect: base;
			grid ChessBoard;
		}
	}
}