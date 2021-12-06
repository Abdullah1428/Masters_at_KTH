/**
* Name: Utility
* Based on the internal empty template. 
* Author: abdullah
* Tags: 
*/


model Utility

/* Insert your model definition here */

global {
	int worldDimension <- 50;
	geometry shape <- square(worldDimension);
	
	list<string> stageAttributes <- ['sound', 'lights', 'visuals', 'decorations', 'area', 'fireworks'];
	list<rgb> stageColors <- [rgb('purple'), rgb('cyan'), rgb('red'), rgb('green')];
	
	init {
		create EventGuest number: 20;
		create EventStage
		{
			location <- {0,25};
			color <- stageColors[0];
		}
		create EventStage
		{
			location <- {50,25};
			color <- stageColors[1];
		}
		create EventStage
		{
			location <- {25,0};
			color <- stageColors[2];
		}
		create EventStage
		{
			location <- {25,50};
			color <- stageColors[3];
		}
	}
	
}

species EventGuest skills: [moving, fipa] {
	
	EventStage stage <- nil;
	point stageLocation <- nil;
	float myUtilityValue <- 0.0;
	
//	map<string, float> guestPreferences <- ['sound'::rnd(0.1,1.0), 'lights'::rnd(0.1,1.0), 'visuals'::rnd(0.1,1.0),
//		'decorations'::rnd(0.1,1.0), 'area'::rnd(0.1,1.0), 'fireworks'::rnd(0.1,1.0)
//	]; // string: preference name, float: preference value between 0.0 and 1.0

	map<string, float> guestPreferences;
	
	rgb color <- rgb('green');
	
	aspect base
	{
		draw geometry:sphere(1) color: color;
	}
	
	init 
	{
		loop attribute over: stageAttributes 
		{
			add attribute::rnd(0.2, 1.0) to: guestPreferences;
		}
	}
	
	float calcUtility(map<string, float> stageConcertAttributes) 
	{
		
		float utilityValue <- 0.0;
		
		loop attribute over: guestPreferences.keys 
		{
			utilityValue <- utilityValue + (guestPreferences[attribute] * stageConcertAttributes[attribute]);
		}
		
		return utilityValue;
	}
	
	reflex wanderInFestival when: stage = nil 
	{
		do wander;
	}
	
	reflex moveToTarget when: stageLocation != nil 
	{
		do goto target: stage.location;
	}
	
	// ask for attributes of the stage
	reflex askForAttributesFromStages when: stage = nil and empty(informs) 
	{
		do start_conversation to: list(EventStage) performative: 'request' contents: ['attributes?'];
	}
	
	reflex getAttributesReceivedFromStages when: stage = nil and !empty(informs) 
	{
		message attributesReceived <- informs at 0;	

		loop attribute over: container(attributesReceived.contents) 
		{
			
			float calculatedUtilityValue <- calcUtility(map<string, float>(attribute));
			if (calculatedUtilityValue > myUtilityValue) 
			{
				write 'The calculated utility value is ' + calculatedUtilityValue + ' for stage ' + EventStage(attributesReceived.sender).name; 
				write 'guest utility value is ' + myUtilityValue; 
				myUtilityValue <- calculatedUtilityValue;
				write 'updated utility value is ' + myUtilityValue; 
				stage <- attributesReceived.sender;
				color <- EventStage(attributesReceived.sender).color;
			}
		} 
		
		if(stage != nil) 
		{
			write name + ' needs to go to stage' + stage + 'base on the utility value';
			do start_conversation to: [stage] performative: 'request' contents: ['location?'];
		}
	}
	
	reflex getStageLocation when: stage != nil and stageLocation = nil and !empty(informs) 
	{
		message responseToMove <- informs at 0;
		
		loop res over: container(responseToMove.contents) 
		{
			stageLocation <- res;
		}
	}
	
	reflex endConcertForGuest when: stage != nil and stageLocation != nil and !empty(informs) 
	{
		message concertEnded <- informs at 0;
		loop ended over: container(concertEnded.contents) 
		{
			if(ended = stage) 
			{
				stage <- nil;
				stageLocation <- nil;
				myUtilityValue <- 0.0;
			}
		}
	}
	
}


species EventStage skills: [fipa] {
	
	rgb color;
	map<string, float> stageConcertAttributes;
	int concertDuration <- 0 update: concertDuration + 1;
	int restartConcert <- 0 update: restartConcert + 1;
	bool concertStarted <- false;
	
	aspect base{
		draw geometry:square(5) color: color;
	}
	
	action assignStageAttributes 
	{
		loop attribute over: stageAttributes 
		{
			add attribute::rnd(0.2, 1.0) to: stageConcertAttributes;
		}
	}
	
	init {
		do assignStageAttributes;
	}
	
	reflex startConcertAtStage when: !concertStarted and restartConcert > 50
	{
		concertDuration <- 0;
		concertStarted <- true;
	}
	
	reflex endConcertAtStage when: concertStarted and concertDuration > 100 
	{
		restartConcert <- 0;
		concertStarted <- false;
		
		stageConcertAttributes <- [];
		do assignStageAttributes;
		
		do start_conversation to: list(EventGuest) performative: 'inform' contents: [self];
		
	}
	
	reflex answerRequestsFromEventGuests when: !empty(requests) 
	{
		
		message attributesRequests <- requests at 0;
		loop request over: container(attributesRequests.contents) 
		{
			if (string(request) = 'attributes?') 
			{
				do inform message: attributesRequests contents: [stageConcertAttributes];
			}
			else if (string(request) = 'location?') 
			{
				do inform message: attributesRequests contents: [self.location];
			}
		}
		
	}
}


experiment individualUtility type: gui 
{
	output 
	{
		display map type: opengl 
		{
			species EventGuest aspect: base;
			species EventStage aspect: base;
		}
	}
}