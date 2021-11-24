/**
* Name: FestivalAuction
* Based on the internal empty template. 
* Author: abdullah
* Tags: 
*/


model AuctionMandatory

/* Insert your model definition here */

global {
	int worldDimension <- 50;
	geometry shape <- square(worldDimension);
	
	init {
		create Auctioneer number: 1
		{
			location <- {25,30};
		}
		create Participant number: 10;
	}
	
}

species Auctioneer skills: [fipa] {
		
	int startingPrice <- 100;
	int minimumPrice <- 50;
	
	float auctionStart <- 0.01;  // like one percent
	
	list<Participant> subscribedParticipants <- [];
	list<Participant> refusedParticipants <- [];
	int reached <- 0;
	
	int auctionPhase <- 0;
	
	aspect base{
		draw geometry:square(4) color: rgb('purple');
	}
	
	// start auction with a probability of 10%
	reflex startAuction when: auctionPhase = 0 and flip(auctionStart) {
		subscribedParticipants <- [];
		refusedParticipants <- [];
		startingPrice <- 100;
		reached <- 0;
		
		write name + ' started dutch auction with starting price of ' + startingPrice;
		
		do start_conversation to: list(Participant) protocol: 'fipa-contract-net' performative: 'inform' contents: ['auction started'];
		
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
	reflex addTheReachedParticipants when: auctionPhase = 1 and empty(subscribes) and !empty(informs) {
		
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
		do start_conversation to: subscribedParticipants protocol: 'fipa-contract-net' performative: 'cfp' contents: [startingPrice];
		
		auctionPhase <- 3;
	}
	
	
	reflex handleRefusalFromParticipant when: auctionPhase = 3 and empty(proposes) and !empty(refuses) {
		
		message refusalFromParticipant <- refuses at 0;
		
		loop refusal over: container(refusalFromParticipant.contents) {
			
			if(string(refusal) = 'out of my range' and !(refusedParticipants contains refusalFromParticipant.sender)) {
				add Participant(refusalFromParticipant.sender) to: refusedParticipants;
			}
		}
	}
	
	reflex lowerAuctionPrice when: auctionPhase = 3  and length(refusedParticipants) = length(subscribedParticipants) {
		
		startingPrice <- startingPrice - 1;
		refusedParticipants <- [];
		
		if(startingPrice < minimumPrice) {
			do start_conversation to: subscribedParticipants protocol: 'fipa-contract-net' performative: 'inform' contents: ['auction ended'];
		
			auctionPhase <- 5;
			
			return;
		}
		
		auctionPhase <- 2;
	}
	
	reflex handleResponseFromParticipant when: auctionPhase = 3 and !empty(proposes) {
		
		message proposalFromParticipant <- proposes at 0;
		
		loop accepted over: container(proposalFromParticipant.contents) {
			
			if(string(accepted) = 'in my range') {
				do accept_proposal message: proposalFromParticipant contents: ['accepted proposal'];
				break;
			}
		}
		
		loop rejected over: container(proposalFromParticipant.contents) {
			if (string(rejected) = 'out of my range') {
				do reject_proposal message: proposalFromParticipant contents: ['rejected proposal'];	
			}
		}
		
		auctionPhase <- 4;
	}
	
	
	reflex announceWinnerOfAuction when: auctionPhase = 4 and !empty(informs) {
		
		message auctionWinner <- informs at 0;
		
		loop deal over: container(auctionWinner.contents) {
			
			
			if(string(deal) = 'deal') {
				write Participant(auctionWinner.sender).name + ' won auction of ' + self.name + ' and has to pay ' + startingPrice;
			}
		}
		
		do start_conversation to: subscribedParticipants protocol: 'fipa-contract-net' performative: 'inform' contents: ['auction ended'];
		
		auctionPhase <- 5;
	}
	
	reflex removeAuctionSubscribers when: auctionPhase = 5 and !empty(subscribes) {
		
		message removeParticipants <- subscribes at 0;
		
		loop unsub over: container(removeParticipants.contents) {
			
			if(string(unsub) = 'unsubscribing') {
				remove removeParticipants.sender from: subscribedParticipants;
			}
		}
	}
	
	reflex endAuction when: auctionPhase = 5 and empty(subscribedParticipants) {
		
		write 'Auction of ' + name + ' ended';
		auctionPhase <- 0;
	}
	
}

species Participant skills: [fipa, moving] {
		
	rgb color <- rgb('green');
	
	int participantMaximumPrice <- rnd(20,90);
	
	Auctioneer auctioneer <- nil;
	
	bool register <- false;
	
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
			
			if (string(content) = 'auction started') {
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
				do propose message: auctionPrice contents: ['in my range'];
				write self.name + ' accepts price of ' + int(price);
				break;
			} else {
				do refuse message: auctionPrice contents: ['out of my range'];
				write self.name + ' refuses price of ' + int(price);
			}
		}
	}
	
	
	reflex handleAcceptedByTheAuctioneer when: auctioneer != nil and !empty(accept_proposals) {
		
		message acceptedProposal <- accept_proposals at 0;
		
		loop msg over: container(acceptedProposal.contents) {
			if(string(msg) = 'accepted proposal') {
				do inform message: acceptedProposal contents: ['deal'];
				color <- #purple;
			}
		}
	}
	
	reflex unsubscribeFromAuction when: auctioneer != nil and !empty(informs) {
		
		message unSubFromAuction <- informs at 0;
		
		loop msg over: container(unSubFromAuction.contents) {
			
			if(string(msg) = 'auction ended') {
				do subscribe message: unSubFromAuction contents: ['unsubscribing'];
				auctioneer <- nil;
				register <- false;
				participantMaximumPrice <- rnd(30,80);
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