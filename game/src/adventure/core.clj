
(ns adventure.core
  (:require [clojure.core.match :refer [match]]
			[clojure.string :as str])
  (:gen-class))

(def axe
  {
  :name "Axe"
  }
)

(def door
  {
  	:name "Door"
  }
)

(def barrier
  {
  	:name "Barrier"
  }
)

(def rollingPin
  {
  :name "Rolling Pin"
  }
)

(def key1
  {
  :name "Key 1"
  }
)

(def key2
  {
  :name "Key 2"
  }
)

(def knife
  {
  :name "Knife"
  }
)

(def sword
  {
  :name "Sword"
  }
)

(def gun
  {
  :name "Gun"
  }
)

(def bullets
  {
  :name "Bullets"
  }
)

(def chest
  {
  :name "Chest"
  }
)

(def fist
  {
  	:name "Fist"
  }
)

(def flashlight
  {
  	:name "Flashlight"
  }
)

(def keyToObjectMap
	{
		;;(:or [:Axe] [:axe]) axe
    [:Axe] axe
    [:axe] axe
		;;(:or [:Door] [:door]) door
    [:Door] door
    [:door] door
		;;(:or [:Barrier] [:barrier]) barrier
    [:Barrier] barrier
    [:barrier] barrier
		;;(:or [:RollingPin] [:rolling :pin]) rollingPin
    [:Rolling :Pin] rollingPin
    [:rolling :pin] rollingPin
		;;(:or [:Key1] [:key :1]) key1
    [:Key1] key1
    [:key :1] key1
		;;(:or [:Key2] [:key :2]) key2
    [:Key2] key2
    [:key :2] key2
		;;(:or [:Knife] [:knife]) knife
    [:Knife] knife
    [:knife] knife
		;;(:or [:Sword] [:sword]) sword
    [:Sword] sword
    [:sword] sword
		;;(:or [:Gun] [:gun]) gun
    [:Gun] gun
    [:gun] gun
		;;(:or [:Bullets] [:bullets]) bullets
    [:Bullets] bullets
    [:bullets] bullets
		;;(:or [:Chest] [:chest]) chest
    [:Chest] chest
    [:chest] chest
    ;;(:or [:Flashlight] [:flashlight]) flashlight
    [:Flashlight] flashlight
    [:flashlight] flashlight
	}
)

(def keyToWeaponMap
	{
    ;;(:or [:Axe] [:axe]) axe
    [:Axe] axe
    [:axe] axe
    ;;(:or [:RollingPin] [:rolling :pin]) rollingPin
    [:Rolling :Pin] rollingPin
    [:rolling :pin] rollingPin
    ;;(:or [:Gun] [:gun]) gun
    [:Gun] gun
    [:gun] gun
    ;;(:or [:Knife] [:knife]) knife
    [:Knife] knife
    [:knife] knife
		;;(:or [:Sword] [:sword]) sword
    [:Sword] sword
    [:sword] sword
	}
)


(def the-map
  {:foyer {:desc "\nA rumble of thunder suddenly wakes you up, you realize you're in a place you've never been before. You find yourself in a large room with pristine, white walls and strange Vicorian esque portrait. To your back you notice a door.\n"
		   :title "in the Foyer. You can only go north up the Stairs or east to the Kitchen"
		   :dir {:east :kitchen
				:north :staircase}
		   :contents #{gun door}}

   :kitchen{  :desc "\nYou notice an uncanny rotting smell with blood spewed all over the floor. There's something on the counter and a weak Barrier in the floor. \n"
			  :title "in the Kitchen. You can go west back to the Foyer"
			  :dir {:west :foyer}
			  :contents #{rollingPin barrier}}

   :room2{  :desc "\nAs you enter room you notice a the lights flickering on and off. And you see tattered sheets that have been sliced up in every which way. Something stuck in the wall, and something is glittering on the ground. \n"
			  :title "in Bedroom 2. You can go east back to the hallway"
			  :dir {:east :hallway}
			  :contents #{key1 knife}}

   :room1{  :desc "\nYou notice something covered by a blanket in the corner and a Chest beside it. It seems like you may need something to open it. \n"
			  :title "in Bedroom 1. You can go east to the Balcony or west back to the Hallway"
			  :dir {:east :balcony
					:west :hallway}
			  :contents #{sword chest}}

   :hallway{  :desc "\nAfter reaching the top of the stairs and see a suit of armor. Freaked out from the laugh earlier, you run to it to try and pick up its sword, but find its missing.\n"
			  :title "in the Hallway at the top of the stairs. You can go west to Bedroom 2, east to Bedroom 1, or south back to the Stairs"
			  :dir {:west :room2
					:east :room1
					:south :staircase}
			  :contents #{bullets}}

   :staircase{  :desc "\nWhile walking up the stairs, you notice how creaky they are and make a comment out loud. But then you hear a bellowing laugh from bellow.\n"
			  :title "in the Staircase. You can go north to the upstairs Hallway or go south back to the Foyer"
			  :dir {:north :hallway
					:south :foyer}
			  :contents #{flashlight}}

   :frontyard{ :desc "\nYou notice a strange creature standing in front of the gate to exit. Suddenly the door behind you disappears and you are stuck. Your only option is to fight the creature. Equip a weapon if you choose, fighting the monster is your only option.\n"
			  :title "in the Frontyard. You cannot leave this room. Type \"fight\" to fight the monster and end the game"
			  :dir {}
			  :contents #{}}

   :tunnel{  :desc "\nYou look around you and notice that you are in a tunnel. A little up ahead you notice a dusty old sign. You wipe it off and read aloud,'The end is near'.\n"
			  :title "in the Tunnel. Type \"escape\" to try to exit the tunnel and end the game."
			  :dir {}
			  :contents #{}}

	:balcony{  :desc "\nYou're finally able get some fresh air, but are unable to find a way down from here. There also seems to be something rustling through the bushes below.\n"
			  :title "on the Balcony overlooking lawn. You can go west back to Bedroom 1"
			  :dir {:west :room1}
			  :contents #{axe}}
   }
)

(def adventurer
  {:location :foyer
   :inventory #{}
   :weapon fist
   :loadedGun false
   :equippedFlashlight false
   :gameOver false
   :tick 0
   :seen #{}
   :canTravel #{}
  }
)

(defn status [player]
  (let [location (player :location)]
  	(if (identical? (player :gameOver) true)
  		(do (println "\n\nGAME OVER") (System/exit 0))
  	)
  ;;(print (str "\nYou are " (-> the-map location :title) ".\n"))

	(when-not ((player :seen) location)
	  (print (-> the-map location :desc)))
  (print (str "\nYou are " (-> the-map location :title) ".\n"))
	(update-in player [:seen] #(conj % location))

  ;;  (do
    ;;  (print (str "\nYou are " (-> the-map location :title) ".\n"))
    ;;)
  )
)

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn go [dir player]
  (let [location (player :location)
		dest (->> the-map location :dir dir)]
	(if (nil? dest)
	  (do (println "You can't go that way.")
		  player)
	  (assoc-in player [:location] dest))))

(defn tock [player]
  (update-in player [:tick] inc))

(defn scanroom [player]
  (let [location (player :location)
		contents (-> the-map location :contents)]

	(if (empty? contents)
	  (do (println "There is nothing in this room") player)

	  (do
		(print "You see: ")
		(loop [it contents]

		  (if (empty? it)
			(do (print "\n") player)

			(do
				(if (nil? ((player :inventory) (first it)))
					(do (print (str ((first it) :name) ", ")))
				)

				(recur (rest it))
			)
		  )
		)
	  )
	)
  )
)

(defn checkInventory [player]
	(let [inventory (player :inventory)]

		(if (empty? inventory)
		  (do (println "No items in inventory") player)

		  (do
			(print "Items In Your Inventory: ")
			(loop [it inventory]

			  (if (empty? it)
				(do (print "\n") player)
				(do (print (str ((first it) :name) ", ") )
				  (recur (rest it))
				)
			  )
			)
		  )
		)
	)
)

(defn pickup [player command]
	(let [_  (print "")
		  ;;command (read-line)
      temp (drop 2 command)
		  obj (keyToObjectMap temp)
		  location (player :location)
		  contents (-> the-map location :contents)]

		(if (nil? obj)
			(do (println (str (obj :name) " is not a valid object")) player)
			; else
			(do
				; If room doesn't have the object
				(if (nil? (contents obj))
					(do (println (str (obj :name) " is not an object in the room")) player)

					; else
					(if (identical? obj chest)
						(do (println "Cannot pickup Chest") player)

						(if (identical? obj door)
							(do (println "Cannot pickup Door") player)

							(if (identical? obj barrier)
								(do (println "Cannot pickup Barrier") player)

								(if (nil? ((player :inventory) obj))
									(do
                    (println (str "You now have " (obj :name)))
                    (update-in player [:inventory] #(conj % obj))
                  )
									(do (println (str "You already have " (obj :name))) player)
								)
							)
						)
					)
				)
			)
		)
	)
)

(defn unlockchest [player]
	(let [location (player :location)]
		(if (identical? location :room1)
			(if(nil? ((player :inventory) key1))
				; In room2 but doesn't have key1
				(do (println "You do not have Key 1, so you cannot open the chest!") player)
				(do
					(println "You received Key 2!")
					(update-in player [:inventory] #(conj % key2))
				)
			)

			; Not in room2
			(do (println "You cannot open any Chests in this room.") player)
		)
	)
)

(defn unlockdoor [player]
	(let [location (player :location)]
		(if (identical? location :foyer)
			(if(nil? ((player :inventory) key2))
				; In foyer but doesn't have key2
				(do (println "You do not have Key 2, so you cannot open the Door!") player)
				(do
					(println "You opened the Door. You can now travel through it!")
					(update-in player [:canTravel] #(conj % door))
				)
			)

			; Not in foyer
			(do (println "You cannot unlock any Doors in this room") player)
		)
	)
)

(defn travelthroughdoor [player]
	(if (nil? ((player :canTravel) door))
		(do (println "You have not unlocked the Door, so you cannot travel through it!") player)

		(if (identical? (player :location) :foyer)
			(assoc-in player [:location] :frontyard)
			(do (println "You cannot travel through the Door from your current room") player)
		)
	)
)


(defn breakbarrier [player]
	(let [location (player :location)]
		(if (identical? location :kitchen)
			(if (identical? (player :weapon) axe)
				; In foyer but doesn't have key2
				(do
					(println "You destroyed the barrier. You see a tunnel that you can travel to!")
					(update-in player [:canTravel] #(conj % barrier))
				)
        (do (println "You have not equipped the Axe, so you cannot break the Barrier!") player)
			)

			; Not in foyer
			(do (println "You cannot break any Barriers in this room") player)
		)
	)
)

(defn travelTunnel [player]
	(if (nil? ((player :canTravel) barrier))
		(do (println "You have not broken through the barrier, so you cannot travel through the tunnel!") player)
		(if (identical? (player :location) :kitchen)
			(assoc-in player [:location] :tunnel)
      (if (identical? (player :location) :tunnel)
        (assoc-in player [:location] :kitchen)
        (do (println "You cannot travel to or from the Tunnel from this room") player)
      )
		)
	)
)

(defn equip [player command]
	(let [_   (print "")
		  ;;command (read-line)
      temp (drop 1 command)
       obj (keyToWeaponMap temp)]

		(if (nil? obj)
			(do (println (str (obj :name) " is not a valid weapon")) player)

			; Was a valid weapon, now check if in inventory
			(if (nil? ((player :inventory) obj))
				(do (println (str "You do not have " (obj :name))) player)
				(do (println (str "You've equipped " (obj :name)))
          (assoc-in player [:weapon] obj))
			)
		)
	)
)

(defn loadBullets [player]
	(if (identical? (player :weapon) gun)
		(if (nil? ((player :inventory) bullets))
			(do (println "You do not have the Bullets") player)
			(assoc-in player [:loadedGun] true)
		)
		(do (println "You have not equipped the Gun, so you can not load it!") player)
	)
)

(defn equipFlashlight [player]

	; Was a valid weapon, now check if in inventory
	(if (nil? ((player :inventory) flashlight))
		(do (println (str "You do not have " (flashlight :name))) player)
		(do (println (str "You've equipped " (flashlight :name)))
      (assoc-in player [:equippedFlashlight] true))
	)
)

(defn fight [player]
	(if (identical? (player :location) :frontyard)
		(do
			(let [weapon (player :weapon)]
        (if (identical? weapon axe)
          (do (println "\nYou swing furiously at the monster, but he counters your every move. Eventually you get tired and the monster has his way with you. A lighter weapon may have been a better choice. You Lose!"))

  				(if (identical? weapon rollingPin)
  					(do (println "\nYou valiantly tried to defeat the monster with the Rolling Pin. Sadly your Rolling Pin couldn't handle him like it did when you made bread last week. You Lose!"))

  					(if (identical? weapon sword)
  						(do (println "\nWait for it..wait for it...Swi-. The Sword but it was too heavy for you. You Lose!"))

  						(if (identical? weapon knife)
  							(do (println "\nYou skillfully countered the monster up everytime he made his attack. Afraid he'd end up like swiss cheese, he ran away in defeat. Looks like middle school fencing classes came in handy. You Win!"))

  							(if (identical? weapon gun)
  								(if (identical? (player :loadedGun) true)
  									(do (println "\nYou shot at the monster mercilessly. Even when it begged for death you didn't stop. By the time you stopped firing the monster was unrecognizable. Makes you wonder who the real moster is. You Win....I guess"))
  									(do (println "\nYou confindently took aim at the monster, pulled the trigger, and ... nothing. The Gun was unloaded, so the monster had his way with you! You Lose!"))
  								)

  								(do (println "\nLooks like you should have been hitting the gym a bit more. Your bare Fists were not enough to handle the monster! You Lose!"))
  							)
  						)
  					)
  				)
        )
			)

			(assoc-in player [:gameOver] true)
		)

		(do (println "There is nothing to fight in this room") player)
	)
)

(defn escape [player]
	(if (identical? (player :location) :tunnel)
    (if (identical? (player :equippedFlashlight) true)
  		(do
  			(println "\nYou were able to use the tunnel to reach safety. You won!!!!")
  			(assoc-in player [:gameOver] true)
  		)
      (do (println "\nYou're the tunnels are too dark for you to safely use. You'll need to equip something to light the way") player)
    )
		(do (println "You cannot escape from this room!") player)
	)
)

(defn help [player]
	(do (println "\nPossible Commands (Some can only be used in some situations):\n\n Directional Commands:\n\tnorth (or w),\n\tsouth(or s),\n\teast(or d),\n\twest(or a)\n\n Actions:\n\tscan, check inventory, pick up (name of object you want to pick up),\n\tunlock chest(or open chest), unlock door,\n\ttravel through door, travel to tunnel, travel from tunnel\n\tbreak barrier, equip (name of object you want to equip),\n\tload bullets, fight, escape\n\nNOTE: Flashlight is not a weapon, so it can be equipped along with a weapon") player)
)


(defn respond [player command]
  (match command
		[:look] (update-in player [:seen] #(disj % (-> player :location)))
		[:north] (go :north player)
    [:w] (go :north player)
		[:south] (go :south player)
    [:s] (go :south player)
		[:east] (go :east player)
    [:d] (go :east player)
		[:west] (go :west player)
    [:a] (go :west player)
		[:scan] (scanroom player)
		(:or [:check :inventory] [:inventory]) (checkInventory player)
		(:or  [:pick :up :gun] [:pick :up :Gun]
          [:pick :up :axe] [:pick :up :Axe]
          [:pick :up :rolling :pin] [:pick :up :RollingPin]
          [:pick :up :key :1] [:pick :up :Key1]
          [:pick :up :key :2] [:pick :up :Key2]
          [:pick :up :sword] [:pick :up :Sword]
          [:pick :up :knife] [:pick :up :Knife]
          [:pick :up :bullets] [:pick :up :Bullets]
          [:pick :up :door] [:pick :up :Door]
          [:pick :up :barrier] [:pick :up :Barrier]
          [:pick :up :chest] [:pick :up :Chest]
          [:pick :up :flashlight] [:pick :up :Flashlight]
      ) (pickup player command)
		(:or [:open :chest] [:unlock :chest]) (unlockchest player)
		[:unlock :door] (unlockdoor player)
		[:travel :through :door] (travelthroughdoor player)
		[:break :barrier] (breakbarrier player)
		[:travel :to :tunnel] (travelTunnel player)
  	[:travel :from :tunnel] (travelTunnel player)
    (:or  [:equip :gun] [:equip :Gun]
          [:equip :axe] [:equip :Axe]
          [:equip :rolling :pin] [:equip :Rolling :Pin]
          [:equip :key :1] [:equip :Key1]
          [:equip :key :2] [:equip :Key2]
          [:equip :sword] [:equip :Sword]
          [:equip :knife] [:equip :Knife]
          [:equip :bullets] [:equip :Bullets]
          [:equip :door] [:equip :Door]
          [:equip :barrier] [:equip :Barrier]
          [:equip :chest] [:equip :Chest]
      ) (equip player command)
    ;;(:or [:equip :Flashlight] [:equip :flashlight]) (equipFlashlight player)
    [:equip :Flashlight] (equipFlashlight player)
    [:equip :flashlight] (equipFlashlight player)
		[:load :bullets] (loadBullets player)
		[:fight] (fight player)
		[:escape] (escape player)
		[:help] (help player)
		_ (do (println "I don't understand you.")
			   player)
		)
)


(defn -main
  "Raajesh, Pratheek, Prashant's Text Based Adventure Game"
  [& args]
  (println "\n\n**************ESCAPE**************\n\tA Game in Clojure\n**********************************\n\n")
  ;;(help adventurer)
  (loop [local-map the-map
		local-player adventurer]
	(let [pl (status local-player)
		  _  (println "\nWhat do you want to do? Type \"help\" for possible commands")
		  command (read-line)]
    (println "\n\n")
	  (recur local-map (respond pl (to-keywords command))))))
