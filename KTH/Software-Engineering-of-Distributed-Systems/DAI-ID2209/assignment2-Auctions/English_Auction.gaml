/**
* Name: EnglishAuction
* Based on the internal empty template. 
* Author: abdullah
* Tags: 
*/


model EnglishAuction

/* Insert your model definition here */

global {
	int worldDimension <- 50;
	geometry shape <- square(worldDimension);
	
	list<string> genres <- ['cloths', 'cds', 'games', 'electronics'];
	list<rgb> genresColors <- [rgb('purple'), rgb('cyan'), rgb('red'), rgb('green')];
	
	init {
		create Auctioneer
		{
			location <- {0,25};
			auctionGenre <- genres[0];
			color <- genresColors[0];
		}
//		create Auctioneer
//		{
//			location <- {50,25};
//			auctionGenre <- genres[1];
//			color <- genresColors[1];
//		}
//		create Auctioneer
//		{
//			location <- {25,0};
//			auctionGenre <- genres[2];
//			color <- genresColors[2];
//		}
//		create Auctioneer
//		{
//			location <- {25,50};
//			auctionGenre <- genres[3];
//			color <- genresColors[3];
//		}
		create Participant number: 10;
	}
	
}

species Auctioneer skills: [fipa] {
	rgb color;			
	int startingPrice <- 1;
	int minimumPrice <- 50;
	
	float auctionStart <- 0.01;  // like one percent
	
	list<Participant> subscribedParticipants <- [];
	list<Participant> refusedParticipants <- [];
	int reached <- 0;
	
	int auctionPhase <- 0;
	
	string auctionGenre;
	
	aspect base{
		draw geometry:square(4) color: color;
	}
	
	// start auction with a probability of 10%
	reflex startAuction when: auctionPhase = 0 and flip(auctionStart) {
		subscribedParticipants <- [];
		refusedParticipants <- [];
		startingPrice <- 1;
		reached <- 0;
		
		write name + ' started english auction with starting price of ' + startingPrice + ' and genre ' + auctionGenre;
		
		do start_conversation to: list(Participant) protocol: 'fipa-contract-net' performative: 'inform' contents: [auctionGenre];
		
		auctionPhase <- 1;
		
	}
	
	// receive auction participants from the subscribe
	reflex receiveAuctionParticipants when: auctionPhase = 1 and !empty(subscribes) {
		
		message participantsSubscriptions <- subscribes at 0;
		
		loop sub over: container(participantsSubscriptions.contents) {
			
			if(string(sub) = 'subscribing') {
				add Participant(participantsSubscriptions.sender) to: subscribedParticipants;
			}
		}
	}
	
	// add the reached participants when they reach the distance of auctioneer
	reflex addTheReachedParticipants when: auctionPhase = 1 and empty(subscribes) and !empty(informs) and !empty(subscribedParticipants) {
		
		message reachedParticipants <- informs at 0;
		
		loop msg over: container(reachedParticipants.contents) {
			
			if(string(msg) = 'reached') {
				reached <- reached + 1;
			}
		}
		
		if(reached = length(subscribedParticipants)) {
			auctionPhase <- 2;
		}
	}
	
	// now we inform all participants about the auction price
	reflex informParticipantsAboutAuctionPrice when: auctionPhase = 2 {
		do start_conversation to: subscribedParticipants protocol: 'fipa-contract-net' performative: 'cfp' contents: [startingPrice + 1];
		
		auctionPhase <- 3;
	}
		
	reflex handleProposalFromParticipant when: auctionPhase = 3 {
		
		loop proposal over: proposes {
			list<unknown> content <- proposal.contents;
			
			if(int(content[0]) > startingPrice) {
				startingPrice <- int(content[0]);
			}
		}
		
		loop refusal over: refuses {
			list<unknown> content <- refusal.contents;
			
			if(content[0] = 'backing out') {
				remove refusal.sender from: subscribedParticipants;
			}
		}
		
		auctionPhase <- 4;
	}
	
	reflex resumeAuctionIfMoreThanOneBids when: auctionPhase = 4 and length(subscribedParticipants) > 1 {
		auctionPhase <- 2;
	}
	
	reflex announceWinnerOfAuction when: auctionPhase = 4 and length(subscribedParticipants) = 1 {
		
		do start_conversation to: subscribedParticipants protocol: 'fipa-contract-net' performative: 'accept_proposal' contents: ['accepted proposal'];
		
		write subscribedParticipants[0].name + ' won auction of ' + self.name + ' and has to pay ' + startingPrice;
		
		auctionPhase <- 5;
	}
	
	reflex removeAuctionSubscribers when: auctionPhase = 5 and !empty(subscribes) {
		
		message removeParticipants <- subscribes at 0;
		
		loop unsub over: container(removeParticipants.contents) {
			
			if(string(unsub) = 'unsubscribing') {
				remove Participant(removeParticipants.sender) from: subscribedParticipants;
			}
		}
	}
	
	reflex endAuction when: auctionPhase = 5 and empty(subscribedParticipants) {
		
		write 'Auction of ' + name + ' ended';
		auctionPhase <- 6;
	}
	
}

species Participant skills: [fipa, moving] {
		
	rgb color <- rgb('black');
	
	int participantMaximumPrice <- rnd(20,90);
	
	Auctioneer auctioneer <- nil;
	
	bool register <- false;
	
	string participantGenre <- genres[0]; //any(genres);
	
	aspect base{
		draw geometry:sphere(1) color: color;
	}
	
	// do party
	reflex doParty when: auctioneer = nil {
		do wander;
	}
	
	// find yourself a auction to subscribe
	reflex findNewAuction when: auctioneer = nil and !empty(informs) {
		
		message newAuction <- informs at 0;
		
		loop content over: container(newAuction.contents) {
			
			if (string(content) = participantGenre) {
				auctioneer <- Auctioneer(newAuction.sender);
				do subscribe message: newAuction contents: ['subscribing'];
			}
		}
			
	}
	
	// move towards the auctioneer.
	reflex moveTowardsAuctioneer when: auctioneer != nil and location distance_to(auctioneer.location) > 2 {
		do goto target: auctioneer.location;
	}
	
	// inform the auctioneer that you have reached them so that they can add you to list
	reflex informAuctioneer when: auctioneer != nil and self.location distance_to auctioneer.location < 3 and !register {
		
		register <- true;
		
		list<Auctioneer> auctionOwner <- [auctioneer];
		do start_conversation to: auctionOwner protocol: 'fipa-contract-net' performative: 'inform' contents: ['reached'];
	}
	
	// recived the auction price
	reflex actOnTheAuctionPrice when: auctioneer != nil and !empty(cfps) and self.location distance_to auctioneer.location < 3 {
		
		message auctionPrice <- cfps at 0;
		
		loop price over: container(auctionPrice.contents)  {
			
			if(int(price) <= self.participantMaximumPrice) {
				do propose message: auctionPrice contents: [int(price)];
				write self.name + ' proposes price of ' + int(price);
			}
			
			if (int(price) > self.participantMaximumPrice) { 
				do refuse message: auctionPrice contents: ['backing out'];
				write self.name + ' refuses price of ' + int(price);
				
				auctioneer <- nil;
				register <- false;
			}
			
		}
	}
	
	
	reflex handleAcceptedByTheAuctioneer when: auctioneer != nil and !empty(accept_proposals) {
		
		message acceptedProposal <- accept_proposals at 0;
		
		loop msg over: container(acceptedProposal.contents) {
			if(string(msg) = 'accepted proposal') {
				do inform message: acceptedProposal contents: ['deal'];
				if (participantGenre = 'cloths')
				{
					color <- genresColors[0];	
				} 
				else if (participantGenre = 'cds') 
				{
					color <- genresColors[1];	
				} 
				else if (participantGenre = 'games') 
				{
					color <- genresColors[2];	
				}
				else if (participantGenre = 'electronics') 
				{
					color <- genresColors[3];	
				}
				
				do subscribe message: msg contents: ['unsubscribing'];
				auctioneer <- nil;
				register <- false; 
			}
		}
	}
	
}


experiment my_auction type: gui {
	output {
		display map type: opengl {
			species Auctioneer aspect: base;
			species Participant aspect: base;
		}
	}
}