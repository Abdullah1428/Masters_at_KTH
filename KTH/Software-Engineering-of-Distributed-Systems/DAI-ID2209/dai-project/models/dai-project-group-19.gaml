/**
* Name: daiprojectgroup19
* Based on the internal empty template. 
* Author: abdullah and aksel
* Tags: 
*/


model daiprojectgroup19

/* Insert your model definition here */

global {
	
	int worldDimension <- 100;
	geometry shape <- square(worldDimension);
	
	// music genres
	list<string> genres <- ['Rock', 'Folk_Music', 'Jazz', 'Metal', 'Classical_Music', 'Pop', 'Other'];
	
	// list of all modules
	list<FestivalModules> festivalModules <- [];
	
	// some global values we can measure
	float generousLimit <- 0.6;
	float incrementGenerousity <- 0.010;
	
	int numberOfDrinkInvitations <- 0;
	int numberOfInteractions <- 0;
	float globalHappiness <- 0.0;
	
	// update global happiness after 10 cycles
	reflex updateGlobalHappiness when: cycle mod 10 = 0 {
		
		globalHappiness <- 0.0;
		
		loop p over: list(PartyAnimal) {
			globalHappiness <- globalHappiness + p.generous;
		}
		loop p over: list(ChillGuy) {
			globalHappiness <- globalHappiness + p.generous;
		}
		loop p over: list(Talkative) {
			globalHappiness <- globalHappiness + p.generous;
		}
		loop p over: list(FoodLover) {
			globalHappiness <- globalHappiness + p.generous;
		}
		loop p over: list(BadGuy) {
			globalHappiness <- globalHappiness + p.generous;
		}
		
		globalHappiness <- globalHappiness / 50;
	}
	
	init 
	{
		// guests
		create PartyAnimal number: 10;
		create ChillGuy number: 10;
		create Talkative number: 10;
		create FoodLover number: 10;
		create BadGuy number: 10;
		
		// modules
		create Pub {
			location <- {0,50};	
			area <- rectangle(10.0, 10.0);
		}
		create ConcertHall {
			location <- {100,50};
			area <- rectangle(10.0, 10.0);
		}
		create Pub {
			location <- {50,0};	
			area <- rectangle(10.0, 10.0);
		}
		create ConcertHall {
			location <- {50,100};
			area <- rectangle(10.0, 10.0);
		}
		festivalModules <- list(Pub) + list(ConcertHall);
	}
	
}


// these two classes can be abstracted means they will act as parents for the type
// of guests and events.
species FestivalGuests skills: [moving, fipa] {
	
	image_file icon <- nil;
	// color for guest agents
	rgb color <- nil;	
	// the module where festival guest will move
	FestivalModules targetModule <- nil;
	// location of the module
	point targetLocation <- nil;
	// go to module value
	float moveToModule <- 0.01; // 1 percent chances going to a module
	// leave module value
	float moveToWandering <- 0.1; // 10 percent chances to back to wandering in festival
	// check if already in some module
	bool alreadyInModule <- false;
	// timer to be in a module 
	int moduleTimer <- 0;
	// invited for drink and food
	bool invitedForDrink <- false;
	float drinkInvitePossibility <- 0.4;
	// activity checker
	bool activityDone <- false;
	bool secondActivityDone <- false;
	
	// guest personl traits
	float generous <- rnd(0.4);
	float noisy <- rnd(0.2);
	float talky <- rnd(0.2);
	float foody <- rnd(0.2);
	float bully <- rnd(0.1);
	
	
	// default shape for all festival guests with color being dynamic from each child agent.
	aspect base
	{
		draw icon size: 5;
	}
	
	// move randomly when there are no informs about the modules
	reflex doWander when: targetModule = nil and empty(informs) 
	{
		do wander;
	}
	
	// move towards the module location
	// and location distance_to(targetLocation) > 2 // commented out for now.
	reflex moveToTarget when: targetLocation != nil and alreadyInModule = false 
	{
		do goto target: targetLocation;
	}
	
	// here a guest randomly selects a module to go to with 1 percent possibility value and
	// then asks for the location of that module
	reflex selectTargetModule when: targetModule = nil and rnd(1.0) <= moveToModule and empty(informs) 
	{	
		targetModule <- any(festivalModules);
		//write self.name + ' chooses to go to ' + targetModule;
		do start_conversation to: [targetModule] performative: 'request' contents: ['send_location'];
	}
	
	// guest receives the location from module and saves it in targetLocation
	reflex recievFestivalModuleLocation when: targetModule != nil and targetLocation = nil and alreadyInModule = false and !empty(informs) 
	{
		loop i over: informs 
		{
			list<unknown> c <- i.contents;
			if(c[0] = 'location') 
			{
				targetLocation <- c[1];
			}
		}
	}
	
	// ask for permission to be entered in the module place either a concert or a bar.
	reflex enterFestivalModule when: targetModule != nil and !alreadyInModule and self.location = targetLocation 
	{	
		do start_conversation to: [targetModule] performative: 'subscribe' contents: ['enter'];
		alreadyInModule <- true;
		invitedForDrink <- false;
		activityDone <-  false;
		secondActivityDone <- false;
		moduleTimer <- 0;
	}
	
	// spending time inside the module place
	reflex enjoyFestivalModule when: alreadyInModule 
	{
		moduleTimer <- moduleTimer + 1;
	}

	// after spending some fixed time guest leaves to move back to being wandering
	reflex goBackToFestivalWandering when: alreadyInModule and moduleTimer > 100 and rnd(1.0) <= moveToWandering 
	{
		do goBackToWander;
	}
	
	action goBackToWander 
	{
		do start_conversation to: [targetModule] performative: 'subscribe' contents: ['exit'];
						
		targetModule <- nil;
		targetLocation <- nil;
		alreadyInModule <- false;
	}
	
	// Inviting for a Drink is based on being generous, 
	// so if a person has generous value higher then 0.6
	// he will invite one fellow for drink
	reflex inviteFellowForDrink when: alreadyInModule and !invitedForDrink 
	{
		
		bool generousLevel <- generous > generousLimit;
		bool doInvite <- rnd(1.0) <= drinkInvitePossibility;
		
		if(generousLevel and doInvite) 
		{
			do start_conversation to: [targetModule] performative: 'cfp'
					contents: ['invite_for_drink'];
		}
		
		invitedForDrink <- true;
	}
	 
	// accept the invitation for a drink from another fellow guest which will also result
	// in incrementing the generous value of 0.010
	reflex acceptDrinkInvitationFromFellowGuest when: !empty(proposes)
	{
		
		bool acceptInvite <- rnd(1.0) >= 0.65;
		
		loop p over: proposes {
			list<unknown> c <- p.contents;
			
			if(c[0] = 'invited_for_drink' and acceptInvite) 
			{
				generous <- min(generous + incrementGenerousity, 1);
				numberOfDrinkInvitations <- numberOfDrinkInvitations + 1;
				write FestivalGuests(c[1]).name + ' increased generous value of ' + self.name + ' to ' + self.generous;
			}
			else
			{
				write FestivalGuests(c[1]).name + ' refused a drink invitation from ' + self.name;
			}
		}
	}
		
}


species PartyAnimal parent: FestivalGuests {
	
	image_file icon <- image_file("../includes/images/party.png");
	
	// a party guy will usually have high noise value
	float noisy <- rnd(0.6,1.0);
	float generous <- rnd(0.2,1.0);
	float talky <- rnd(0.2,1.0);
	float foody <- rnd(0.2,1.0);
	float bully <- rnd(0.2,1.0);
	
	list<FestivalGuests> myFriends <- [];
	
	// concert should be noisy
	float thresholdForNoise <- 0.6;
	
	// each party guest have 2 random fav genres
	list<string> favouriteMusicGenres <- [any(genres), any(genres)];
	
	// guest asks for a genre when in concert hall
	reflex askForGenre when: alreadyInModule and targetModule.typeOfModule = 'concert' and !activityDone
	{
		do start_conversation to: [targetModule] performative: 'query' contents: ['concert_genre?'];
		activityDone <- true;
	}
	
	// guest asks for guest list from the module
	reflex askForFellows when: alreadyInModule and !secondActivityDone
	{
		do start_conversation to: [targetModule] performative: 'request' contents: ['send_list'];
		secondActivityDone <- true;
	}
	
	// guest receives genre info from module
	reflex receiveGenre when: alreadyInModule and !empty(queries) 
	{
		
		loop q over: queries {
			list<unknown> c <- q.contents;
			if(c[0] = 'concert_genre_info') 
			{
				do leaveEventIfMusicIsBad receivedGenre: c[1];
			}
		}
	}
	
	// receive guest list
	reflex receiveFellows when: alreadyInModule and !empty(informs) 
	{
		loop i over: informs 
		{
			list<unknown> c <- i.contents;
			if(c[0] = 'guest_list') {
				do leaveIfTooQuiet guests: c[1];
			}
		}
	}
	
	// method to decide if genre is not one of guest favs then guest will leave event.
	action leaveEventIfMusicIsBad(string receivedGenre) 
	{
		write self.name + ' received the music genre ' + receivedGenre;
		bool likeReceivedGenre <- favouriteMusicGenres contains receivedGenre;
		if(!likeReceivedGenre) 
		{
			write self.name + ' does not like music in ' + targetModule.name + ' and left';
			do goBackToWander;
		}
	}
	
	// rule for party animal who only interacts with others if the place is noisy.
	action leaveIfTooQuiet(list<FestivalGuests> guests) 
	{
		float noiseLevel <- 0.0;
		loop g over: guests 
		{
			noiseLevel <- noiseLevel + g.noisy;
		}
		noiseLevel <- noiseLevel / length(guests);
		if(noiseLevel < thresholdForNoise) 
		{
			write self.name + ' finds it too quiet in ' + targetModule.name + ' and left';
			do goBackToWander;
		} 
		else 
		{
			do findAFriend guests: guests;
		}
	}
	
	// finds a random friend from the party.
	action findAFriend(list<FestivalGuests> guests) 
	{
		FestivalGuests randomFriend <- any(guests);
		add randomFriend to: myFriends;
		write 'found a friend named ' + randomFriend.name;
		numberOfInteractions <- numberOfInteractions + 1;
	}
	
}

species ChillGuy parent: FestivalGuests {
	
	image_file icon <- image_file("../includes/images/chill.png");
	
	float noisy <- rnd(0.2,1.0);
	float generous <- rnd(0.35,1.0);
	float talky <- rnd(0.2,1.0);
	float foody <- rnd(0.2,1.0);
	float bully <- rnd(0.2,1.0);
	
	// module should not be more noisy then 0.4 value for the guest.
	float maximumAcceptedNoiseLevel <- 0.4;
	
	// guest asks for guest list when in module
	reflex askForFellows when: alreadyInModule and !activityDone
	{
		do start_conversation to: [targetModule] performative: 'request' contents: ['send_list'];
		activityDone <- true;
	}
	
	// guest recievs fellow guest list
	reflex receiveFellows when: alreadyInModule and !empty(informs) 
	{
		loop i over: informs 
		{
			list<unknown> c <- i.contents;
			if(c[0] = 'guest_list') {
				do leaveIfOtherGuestsAreTooNoisy guests: c[1];
			}
		}
	}
	
	action leaveIfOtherGuestsAreTooNoisy(list<FestivalGuests> guests) 
	{
		float noiseLevel <- 0.0;
		loop g over: guests 
		{
			noiseLevel <- noiseLevel + g.noisy;
		}
		noiseLevel <- noiseLevel / length(guests);
		if(noiseLevel > maximumAcceptedNoiseLevel) 
		{
			write self.name + ' finds it too noisy in ' + targetModule.name + ' and left';
			do goBackToWander;
		}
		else 
		{
			write self.name + ' says ' + targetModule.name + ' is nice and he can enjoy with all these people';
			numberOfInteractions <- numberOfInteractions + 1;
		}
	}
	
}

species Talkative parent: FestivalGuests {
	
	image_file icon <- image_file("../includes/images/talky.png");
		
	rgb color <- rgb('yellow');
	
	float noisy <- rnd(0.2,1.0);
	float generous <- rnd(0.2,1.0);
	float talky <- rnd(0.5,1.0);
	float foody <- rnd(0.2,1.0);
	float bully <- rnd(0.2,1.0);
	
	float minimumAcceptedTalkingLevel <- 0.4;
	
	list<FestivalGuests> myFriends <- [];
	
	reflex askForFellows when: alreadyInModule and !activityDone 
	{
		do start_conversation to: [targetModule] performative: 'request' contents: ['send_list'];
		activityDone <- true;
	}
	
	reflex receiveFellows when: alreadyInModule and !empty(informs) 
	{
		loop i over: informs 
		{
			list<unknown> c <- i.contents;
			if(c[0] = 'guest_list') {
				do leaveIfOtherGuestsAreTooShy guests: c[1];
			}
		}
	}
	
	action leaveIfOtherGuestsAreTooShy(list<FestivalGuests> guests) 
	{
		float talkingLevel <- 0.0;
		loop g over: guests 
		{
			talkingLevel <- talkingLevel + g.talky;
		}
		talkingLevel <- talkingLevel / length(guests);
		if(talkingLevel < minimumAcceptedTalkingLevel) 
		{
			write self.name + ' finds it too shy in ' + targetModule.name + ' and left';
			do goBackToWander;
		}
		else 
		{
			do findOneForAChat guests: guests;
		}
	}
	
	// find the guy who talks the most and start talking 
	action findOneForAChat (list<FestivalGuests> guests) 
	{
		FestivalGuests talkyFriend <- nil;
		float talkValue <- 0.0;
		loop g over: guests 
		{
			if (g.talky > talkValue) 
			{
				talkValue <- g.talky;
				talkyFriend <- g;
			}
		}
		
		add talkyFriend to: myFriends;
		numberOfInteractions <- numberOfInteractions + 1;
		write 'found a talky friend named ' + talkyFriend.name;
	}
  	
}

species FoodLover parent: FestivalGuests {
	
	image_file icon <- image_file("../includes/images/food.png");
		
	float noisy <- rnd(0.2,1.0);
	float generous <- rnd(0.2,1.0);
	float talky <- rnd(0.2,1.0);
	float foody <- rnd(0.5,1.0);
	float bully <- rnd(0.2,1.0);
	
	bool findSomeOneToEatFoodTogether <- false;
	
	list<FestivalGuests> myFriends <- [];
	
	
	reflex askIfPubOpen when: alreadyInModule and targetModule.typeOfModule = 'pub' and !activityDone
	{
		do start_conversation to: [targetModule] performative: 'query' contents: ['pub?'];
		activityDone <- true;
	}
	
	reflex receivePubInformation when: alreadyInModule and !empty(queries) 
	{
		loop q over: queries 
		{
			list<unknown> c <- q.contents;
			if(c[0] = 'pub_info') 
			{
				do decideToStay pubOpen: bool(c[1]) foodQuality: float(c[2]);
			}
		}
	}
	
	action decideToStay (bool pubOpen, float foodQuality) 
	{	
		float hungry <- rnd(0.2,1.0);	
		if (foodQuality >= 0.7 and foody >= 0.6 and hungry >= 0.7) 
		{
			write self.name + ' I am hungry and food looks nice at ' + targetModule.name + ', I can eat and talk to someone here now';
			findSomeOneToEatFoodTogether <- true;
		} 
		else 
		{
			write self.name + ' says food is not good at ' + targetModule.name + ' so no reason to stay';
			do goBackToWander;
		}
		
	}
	
	reflex askForFellows when: alreadyInModule and !secondActivityDone
	{
		do start_conversation to: [targetModule] performative: 'request' contents: ['send_list'];
		secondActivityDone <- true;
	}
	
	reflex receiveFellows when: alreadyInModule and !empty(informs) 
	{
		loop i over: informs 
		{
			list<unknown> c <- i.contents;
			if(c[0] = 'guest_list') 
			{
				do findSomeOneToEatTogether guests: c[1];
			}
		}
	}
	
	action findSomeOneToEatTogether (list<FestivalGuests> guests) 
	{
		FestivalGuests foodyFriend <- nil;
		float foodyValue <- 0.0;
		loop g over: guests 
		{
			if (g.foody > foodyValue) 
			{
				foodyValue <- g.foody;
				foodyFriend <- g;
			}
		}
		
		if (foodyValue > 0.5) {
			add foodyFriend to: myFriends;
			numberOfInteractions <- numberOfInteractions + 1;
			write 'found a foody friend to eat with named ' + foodyFriend.name;			
		} else {
			write 'found no one but guess I can eat alone as food is good';		
		}
		
	}
	
}


species BadGuy parent: FestivalGuests {
	
	image_file icon <- image_file("../includes/images/sauran.png");
	
	float noisy <- rnd(0.2,1.0);
	float generous <- rnd(0.2,1.0);
	float talky <- rnd(0.2,1.0);
	float foody <- rnd(0.2,1.0);
	float bully <- rnd(0.5,1.0);
	
	reflex askIfPubOpen when: alreadyInModule and targetModule.typeOfModule = 'pub' and !activityDone
	{
		do start_conversation to: [targetModule] performative: 'query' contents: ['pub?'];
		activityDone <- true;
	}
	
	reflex receivePubInformation when: alreadyInModule and !empty(queries) 
	{
		
		loop q over: queries 
		{
			list<unknown> c <- q.contents;
			
			if(c[0] = 'pub_info') 
			{
				do startRiotAndLeave pubOpen: bool(c[1]);
			}
		}
	}
	
	reflex askForFellows when: alreadyInModule and !secondActivityDone
	{
		do start_conversation to: [targetModule] performative: 'request' contents: ['send_list'];
		secondActivityDone <- true;
	}
	
	reflex receiveFellows when: alreadyInModule and !empty(informs) 
	{
		loop i over: informs 
		{
			list<unknown> c <- i.contents;
			if(c[0] = 'guest_list') 
			{
				do startRiotAndLeave pubOpen: true;
			}
		}
	}
	
	action startRiotAndLeave(bool pubOpen) 
	{
		
		bool bullyEnough <- bully >= 0.7;
		
		if(pubOpen and bullyEnough) 
		{
			write self.name + ' started riot in ' + targetModule.name + ' which results in loss of global happiness';
			do reduceGlobalHappinessAndFriendShips;		
			do goBackToWander;
		}
	}
	
	action reduceGlobalHappinessAndFriendShips {
		globalHappiness <- globalHappiness - 0.1;
		if (numberOfInteractions > 2) {
			numberOfInteractions <- numberOfInteractions - 2;
		} else {
			numberOfInteractions <- numberOfInteractions - 1;	
		}
	}
	
}


species FestivalModules skills: [fipa] {
	
	image_file icon <- nil;
	geometry area;
	// color for each module
	rgb color <- nil;
	// list for all guests that visits any module in the festival
	list<FestivalGuests> festivalGuests <- [];
	// a timer to restart concerts with different music genres and change restaurant servings.
	int timer <- 0;
	// type of festival module
	string typeOfModule <- nil;
	
	
	// default shape for all festival modules with color being dynamic from each child agent.
	aspect base
	{
		draw area color: color;
		
		draw icon size: 4.5;
	}
	// incrementing time by one.
	reflex clockTicking {
		timer <- timer + 1;
	}
	
	// handle location requests from guests for the module locations
	reflex handleRequestsFromFestivalGuests when: !empty(requests) 
	{
		
		loop r over: requests {
			list<unknown> c <- r.contents;
			
			if(c[0] = 'send_location')
			{
				do inform message: r contents: ['location', self.location];
			}
			else if(c[0] = 'send_list') {
				remove r.sender from: festivalGuests;
				do inform message: r contents: ['guest_list', festivalGuests];
				add r.sender to: festivalGuests;
			}
		}
	}
	
	// handle subscription of guests and add them to guest list
	reflex festivalGuestsEntranceToModule when: !empty(subscribes) 
	{
		loop s over: subscribes 
		{
			list<unknown> c <- s.contents;
			if(c[0] = 'enter') 
			{
				add FestivalGuests(s.sender) to: festivalGuests;
			}
			else if(c[0] = 'exit') 
			{
				remove FestivalGuests(s.sender) from: festivalGuests;
			}
		}
	}
		
	// handle invitation requests for drinks by choosing a random guest from the list
	reflex sendInvitationForDrink when: !empty(cfps) 
	{	
		loop call over: cfps 
		{
			list<unknown> c <- call.contents;
			
			if(c[0] = 'invite_for_drink' and length(festivalGuests) > 1) 
			{
				FestivalGuests chosenGuest <- chooseGuestForDrink(call.sender);
				do start_conversation to: [chosenGuest] performative: 'propose'
						contents: ['invited_for_drink', call.sender];
			}
		}
	}
	
	// this method returns a random guest from the guest list
	FestivalGuests chooseGuestForDrink(FestivalGuests me) 
	{
		remove me from: festivalGuests;
		FestivalGuests guest <- any(festivalGuests);
		add me to: festivalGuests;
		return guest;
	}	
}

species Pub parent: FestivalModules {
	
	image_file icon <- image_file("../includes/images/pub.png");
	
	rgb color <- rgb('black');
	list<rgb> colors <- [rgb('purple'), rgb('cyan'), rgb('red'), rgb('green')];
	
	string typeOfModule <- 'pub';
	
	bool moduleOpen <- false;
	int pubOpenTimer <- 100;
	int pubCloseTimer <- 50;
	
	int lightsCounter <- 0;
	
	float foodQuality <- 0.0;
		
	reflex openPub when: !moduleOpen and timer >= pubCloseTimer 
	{
		moduleOpen <- true;
		lightsCounter <- 0;
		timer <- 0;
		foodQuality <- rnd(0.1,1.0);
		write self.name + ' opened pub';
	}
	
	reflex pubLights when: moduleOpen {
		if (lightsCounter = 4) {
			lightsCounter <- 0;
		}
		color <- colors[lightsCounter];
		lightsCounter <- lightsCounter + 1;
	}
	
	reflex closePub when: moduleOpen and timer >= pubOpenTimer 
	{
		moduleOpen <- false;
		lightsCounter <- 0;
		timer <- 0;
		write self.name + ' closed pub';
	}
	
	reflex informAboutPub when: !empty(queries) 
	{
		loop q over: queries 
		{
			list<unknown> c <- q.contents;
			if(c[0] = 'pub?')
			{
				do query message: q contents: ['pub_info', moduleOpen, foodQuality];
			}
		}
	}
	
}


species ConcertHall parent: FestivalModules {
	
	image_file icon <- image_file("../includes/images/concert.png");
	
	rgb color <- rgb('cyan');
	
	string typeOfModule <- 'concert';
	
	int concertTimer <- 100;
	string concertGenre <- any(genres);
	
	bool moduleOpen <- false;
	
	reflex startConcert when: timer > concertTimer 
	{
		timer <- 0;
		moduleOpen <- true;
		concertGenre <- any(genres);
		
		write self.name + ' started concert with genre ' + concertGenre;
	}
	
	reflex informAboutMusicGenre when: !empty(queries) 
	{
		loop q over: queries 
		{
			list<unknown> c <- q.contents;
			if(c[0] = 'concert_genre?') 
			{
				do query message: q contents: ['concert_genre_info', concertGenre];
			}
		}
	}
	
}


experiment projectDAI type: gui 
{
	output 
	{
		display map type: opengl 
		{
			species PartyAnimal aspect: base;
			species ChillGuy aspect: base;
			species Talkative aspect: base;
			species FoodLover aspect: base;
			species BadGuy aspect: base;
			
			species Pub aspect: base;
			species ConcertHall aspect: base;
		}
		display globalHappinessChart 
		{
			chart 'Global Happiness in Festival' type: series 
			{
				data 'Global Happiness' value: globalHappiness color: #red;
			}
		}
		display globalDrinkInvitations 
		{
			chart 'Drink Invitations In Festival' type: series 
			{
				data 'Number of Drink Invitations' value: numberOfDrinkInvitations color: #blue;
			}
		}
		display globalFriendlyInteractions
		{
			chart 'Friendly Meetings In Festival' type: series 
			{
				data 'Number of Friendly Interactions' value: numberOfInteractions color: #blue;
			}
		}
	}
}