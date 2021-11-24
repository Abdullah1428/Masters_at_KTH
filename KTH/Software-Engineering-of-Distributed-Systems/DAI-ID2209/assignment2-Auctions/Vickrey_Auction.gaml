/**
* Name: VickreyAuction
* Based on the internal empty template. 
* Author: abdullah
* Tags: 
*/


model VickreyAuction

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
		
	int auctionPrice <- 0;
	int secondAuctionPrice <- 0;
	
	float auctionStart <- 0.01;  // like one percent
	
	list<Participant> subscribedParticipants <- [];
	list<Participant> refusedParticipants <- [];
	int reached <- 0;
	
	int auctionPhase <- 0;
	
	bool sealBid <- false;
	
	aspect base{
		draw geometry:square(4) color: rgb('purple');
	}
	
	// start auction with a probability of 10%
	reflex startAuction when: auctionPhase = 0 and empty(subscribedParticipants) and flip(auctionStart) {
		subscribedParticipants <- [];
		refusedParticipants <- [];
		reached <- 0;
		
		write name + ' started vickrey bid auction';
		
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
	reflex informParticipantsAboutAuctionPrice when: auctionPhase = 2 and sealBid = false {
		do start_conversation to: subscribedParticipants protocol: 'fipa-contract-net' performative: 'cfp' contents: [auctionPrice];
		
		auctionPhase <- 3;
	}
	
	
	reflex handleResponseFromParticipant when: auctionPhase = 3 and !empty(proposes) and sealBid = false {
		auctionPrice <- 0;
		
		list<unknown> pro <- proposes;
		list<unknown> con <- [];
		
		loop bid over: pro {
			con <- bid.contents;
			if (int(con[0]) > auctionPrice) {
				secondAuctionPrice <- auctionPrice;
				auctionPrice <- int(con[0]);
				write 'The highest bid is ' + auctionPrice;
			}
		}
		
		loop bid over: pro {
			con <- bid.contents;
			if (int(con[0]) = auctionPrice) {
				do accept_proposal message: bid contents: ['accepted proposal'];
				sealBid <- true;
				break;
			}
		}
		
		loop rejected over: proposes {
			do reject_proposal message: rejected contents: ['rejected proposal'];
		}
	}
	
	
	reflex announceWinnerOfAuction when: auctionPhase = 3 and !empty(informs) and sealBid = true {
		
		message auctionWinner <- informs at 0;
		
		loop deal over: container(auctionWinner.contents) {
			
			if(string(deal) = 'deal') {
				write Participant(auctionWinner.sender).name + ' won auction of ' + self.name + ' and has to pay ' + secondAuctionPrice;
			}
		}
		
		do start_conversation to: subscribedParticipants protocol: 'fipa-contract-net' performative: 'inform' contents: ['auction ended'];
		
		auctionPhase <- 4;
	}
	
	reflex removeAuctionSubscribers when: auctionPhase = 4 and !empty(subscribes) {
		
		message removeParticipants <- subscribes at 0;
		
		loop unsub over: container(removeParticipants.contents) {
			
			if(string(unsub) = 'unsubscribing') {
				remove removeParticipants.sender from: subscribedParticipants;
			}
		}
	}
	
	reflex endAuction when: auctionPhase = 4 and empty(subscribedParticipants) {
		
		write 'Auction of ' + name + ' ended';
		sealBid <- false;
		auctionPhase <- 0;
		auctionPrice <- 0;
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
			do propose message: auctionPrice contents: [participantMaximumPrice];
			write self.name + ' proposes price of ' + participantMaximumPrice;
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