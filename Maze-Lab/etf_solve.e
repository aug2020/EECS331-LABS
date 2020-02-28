note
	description: ""
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	ETF_SOLVE
inherit
	ETF_SOLVE_INTERFACE
create
	make
feature -- command
	solve
    	do
			-- perform some update on the model state
			model.solve
			etf_cmd_container.on_change.notify ([Current])
    	end

end
