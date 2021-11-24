/**
* Name: FesCha1
* Based on the internal empty template. 
* Author: abdullah
* Tags: 
*/


model FesCha1

/* Insert your model definition here */

global {
	
	int worldDimension <-  50;
	geometry shape <- square(worldDimension);
	
	int numOfGuests <- 4;
	int numOfRestaurants <- 2;
	int numOfPubs <- 2;
	int numOfCombine <- 2;
	int numOfInfoCenters <- 1;
	
	
	list<point> pLocation <- [{25,20},{10,40}];
	list<point> rLocation <- [{30,10}, {40,10}];
	list<point> cLocation <- [{20,40}, {15,30}];
	int storeIndexR <- 0;
	int storeIndexP <- 0;
	int storeIndexC <- 0;
	
	float totalDistance <- 0.0;
	float totalDistanceWithMemory <- 0.0;

	
	init {
		create InformationCenter number: numOfInfoCenters
		{
			location <- {(worldDimension - 50), (worldDimension/2)};
		}
		
		create Pub number: numOfPubs
		{
			location <- pLocation at storeIndexP;
			storeIndexP <- storeIndexP + 1;
		}
		
		create Restaurant number: numOfRestaurants
		{
			location <- rLocation at storeIndexR;
			storeIndexR <- storeIndexR + 1;
		}
		
		create CombinePubRestaurant number: numOfCombine
		{
			location <- cLocation at storeIndexC;
			storeIndexC <- storeIndexC + 1;
		}
		
		create Guest number: numOfGuests;
		create GuestWithBrain number: numOfGuests;
	}
}

species Guest skills: [moving] {
	int size <- 1;
	rgb color <- #black;
	int thirst;
	int hunger;
	
	InformationCenter infoCenter <-nil;	
	point targetpoint <- nil;
	
	init {
		thirst <- rnd(1000);
		hunger <- rnd(1000);
	}
	
	aspect base {
		
		rgb agentColor <- rgb('black');
		
		if (thirst < 10) {
			agentColor <- rgb('green');
		}
		if (hunger < 10) {
			agentColor <- rgb('yellow');
		}
		
		if (thirst < 10 and hunger < 10) {
			agentColor <- rgb('purple');
		}
		
		draw geometry:sphere(size) color: agentColor border: agentColor;
	}
	
	reflex beIdle when: infoCenter = nil and targetpoint = nil{
		do wander;	
		color <- #black;
	}
	
	
	reflex lookInformationCenter when:infoCenter = nil and (thirst < 2 or hunger < 2 or (thirst < 2 and hunger < 2)) {
		ask InformationCenter {
			myself.infoCenter <- self;
		}
	}
	
	reflex goToInformationCenter when:infoCenter != nil and targetpoint = nil {
		do goto target:infoCenter;
		
		totalDistance <- totalDistance + 1;
	}
	
	reflex askForDesireFromInfoCenter when: infoCenter != nil and location distance_to(infoCenter) = 0 {
		ask infoCenter {
			if myself.targetpoint = nil {
				
				if myself.thirst < 10 and myself.hunger < 10 {
					write "thirsty and hungry.";
					int rndIndex <- rnd(1);
					myself.targetpoint <- self.combineLocations at rndIndex;
				}
				else if myself.thirst < 10 {
					write "thirsty.";
					int rndIndex <- rnd(1);
					myself.targetpoint <- self.pubLocations at rndIndex;
				}
				else if myself.hunger < 10 {
					write "hungry.";
					int rndIndex <- rnd(1);
					myself.targetpoint <- self.restaurantLocations at rndIndex;
				}
			}
		}
	}
	
	reflex moveToLocation when:targetpoint != nil {
	  do goto target:targetpoint;
	  
	  totalDistance <- totalDistance + 1;
	}
	
	reflex enterPubRestaurant when: targetpoint != nil and location distance_to(targetpoint) = 0 {
		
		ask Pub at_distance (0) {
			write "relieved after drinking, lets party now";
			myself.thirst <- rnd(1000);
		}
		
		ask Restaurant at_distance (0) {
			write "relieved after eating, lets party now";
			myself.hunger <- rnd(1000);
		}
		
		ask CombinePubRestaurant at_distance (0) {
			write "relieved after eating and drinking, lets party now";
			myself.thirst <- rnd(1000);
			myself.hunger <- rnd(1000);
		}
		
		targetpoint <- nil;
		infoCenter <- nil;
	}
	
	reflex enjoyPartyTillAgainThirstorHunger when: targetpoint = nil or location distance_to(targetpoint) != 0 {
		thirst <- (thirst - 1);	
		hunger <- (hunger - 1);	
	}
		
}

species GuestWithBrain skills: [moving] {
	int size <- 1;
	rgb color <- #black;
	int thirst;
	int hunger;
	
	// defining brains for festival guest
	list<point> restaurantBrain <- [];
	list<point> pubBrain <- [];
	list<point> combineBrain <- [];
	
	InformationCenter infoCenter <-nil;	
	point targetpoint <- nil;
	
	init {
		thirst <- rnd(1000);
		hunger <- rnd(1000);
	}
	
	aspect base {
		
		rgb agentColor <- rgb('black');
		
		if (thirst < 10) {
			agentColor <- rgb('green');
		}
		if (hunger < 10) {
			agentColor <- rgb('yellow');
		}
		
		if (thirst < 10 and hunger < 10) {
			agentColor <- rgb('purple');
		}
		
		draw geometry:sphere(size) color: agentColor border: agentColor;
	}
	
	reflex beIdle when: infoCenter = nil{
		do wander;	
		color <- #black;
	}
	
	
	reflex lookInformationCenter when:infoCenter = nil and (thirst < 2 or hunger < 2 or (thirst < 2 and hunger < 2)) {
		ask InformationCenter {
			myself.infoCenter <- self;
		}
	}
	
	reflex goToInformationCenter when:infoCenter != nil and targetpoint = nil {
		do goto target:infoCenter;
		
		totalDistanceWithMemory <- totalDistanceWithMemory + 1;
	}
	
	reflex askForDesireFromInfoCenter when: infoCenter != nil and location distance_to(infoCenter) = 0 {
		ask infoCenter {
			if myself.targetpoint = nil {
				
				if myself.thirst < 10 and myself.hunger < 10 {
					write "thirsty and hungry.";
					int rndIndex <- rnd(1);
					myself.targetpoint <- self.combineLocations at rndIndex;
				}
				else if myself.thirst < 10 {
					write "thirsty.";
					int rndIndex <- rnd(1);
					myself.targetpoint <- self.pubLocations at rndIndex;
				}
				else if myself.hunger < 10 {
					write "hungry.";
					int rndIndex <- rnd(1);
					myself.targetpoint <- self.restaurantLocations at rndIndex;
				}
			}
		}
	}
	
	reflex moveToLocation when:targetpoint != nil {
	  do goto target:targetpoint;
	  
	  totalDistanceWithMemory <- totalDistanceWithMemory + 1;
	}
	
	reflex enterPubRestaurant when: targetpoint != nil and location distance_to(targetpoint) = 0 {
		
		ask Pub at_distance (0) {
			write "relieved after drinking, lets party now";
			myself.thirst <- rnd(1000);
			if (!(myself.targetpoint in myself.pubBrain)) {
				add myself.targetpoint to: myself.pubBrain; // adding to guest brain	
			}
		}
		
		ask Restaurant at_distance (0) {
			write "relieved after eating, lets party now";
			myself.hunger <- rnd(1000);
			if (!(myself.targetpoint in myself.restaurantBrain)) {
				add myself.targetpoint to: myself.restaurantBrain;
			}
		}
		
		ask CombinePubRestaurant at_distance (0) {
			write "relieved after eating and drinking, lets party now";
			myself.thirst <- rnd(1000);
			myself.hunger <- rnd(1000);
			if (!(myself.targetpoint in myself.combineBrain)) {
				add myself.targetpoint to: myself.combineBrain;
			}
		}
		
		targetpoint <- nil;
		infoCenter <- nil;
	}
	
	reflex enjoyPartyTillAgainThirstorHunger when: targetpoint = nil or location distance_to(targetpoint) != 0 {
		thirst <- (thirst - 1);	
		hunger <- (hunger - 1);	
	}
	
	
	reflex useBrainForLocation when: (restaurantBrain != [] or pubBrain != [] or combineBrain != []) and (thirst < 10 or hunger < 10 or (thirst < 10 and hunger < 10)) and infoCenter = nil and targetpoint = nil{
		bool useBrain <- flip(0.5);
			
		if (useBrain = true and thirst < 10 and hunger < 10 and combineBrain != []) {
			write "using brain to go to combine place!!!";
			targetpoint <- combineBrain[rnd(length(combineBrain) - 1)]; // choose a random memory location
			color <- #blue;		
		}
		else if(useBrain = true and thirst <= hunger and pubBrain != []){
			write "using brain to go to pub !!!";
			targetpoint <- pubBrain[rnd(length(pubBrain) - 1)]; // choose a random memory location
			color <- #blue;			
		}
		else if(useBrain = true and !(thirst <= hunger) and restaurantBrain != []){
			write "using brain to go to restaurant!!!";
			targetpoint <- restaurantBrain[rnd(length(restaurantBrain) - 1)]; // choose a random memory location
			color <- #yellow;
		}
		else{
			write "not in brain";
			ask InformationCenter {
				myself.infoCenter <- self;
			}
		}
	}
	
}

species InformationCenter {
	
	list<point> pubLocations <- pLocation;
	list<point> restaurantLocations <- rLocation;
	list<point> combineLocations <- cLocation;
	
	aspect base {
		draw cube(5) color: #red ;
	}
}

species Restaurant {
	aspect base {
		draw triangle(3) color: #yellow ;
	}
}

species Pub {
	aspect base {
		draw triangle(3) color: #green ;
	}
}

species CombinePubRestaurant {
	aspect base {
		draw pyramid(3) color: #purple ;
	}
}


experiment myExperiment type:gui {
	output {
		display map type: opengl {
			species Guest aspect:base;
			species GuestWithBrain aspect:base;
			species InformationCenter aspect:base;
			species Pub aspect: base;
			species Restaurant aspect: base;
			species CombinePubRestaurant aspect: base;
		}
		display chart {
			chart "Festival guest information"{
				data "Distance traveled wihtout memory" value: totalDistance color: #red;
				data "Distance traveled with memory" value: totalDistanceWithMemory color: #blue;
			}
		}
	}
}

