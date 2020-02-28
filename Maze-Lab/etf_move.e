note
	description: ""
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	ETF_MOVE
inherit
	ETF_MOVE_INTERFACE
create
	make
feature -- command
	move(a_direction: INTEGER_32)
		require else
			move_precond(a_direction)
		local
			du: DIRECTION_UTILITY
    	do
			-- perform some update on the model state
			if a_direction ~ N then
				model.move(du.n)
			elseif a_direction ~ E then
				model.move (du.e)
			elseif a_direction ~ W then
				model.move (du.w)
			elseif a_direction ~ S then
				model.move (du.s)
			end

			etf_cmd_container.on_change.notify ([Current])
    	end

end
