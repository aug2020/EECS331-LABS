note
	description: ""
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	ETF_NEW_GAME
inherit
	ETF_NEW_GAME_INTERFACE
create
	make
feature -- command
	new_game(a_level: INTEGER_32)
		require else
			new_game_precond(a_level)
		local
			num: INTEGER
    	do
			-- perform some update on the model state
			if a_level ~ easy then
				num:=1
			elseif  a_level ~ medium then
				num:=2
			elseif a_level ~ hard then
				num:=3
			end
			model.new_game(num)
			etf_cmd_container.on_change.notify ([Current])
    	end

end
