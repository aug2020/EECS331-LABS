note
	description: "A default business model."
	author: "Jackie Wang"
	date: "$Date$"
	revision: "$Revision$"

class
	ETF_MODEL

inherit
	ANY
		redefine
			out
		end

create {ETF_MODEL_ACCESS}
	make

feature {NONE} -- Initialization
	make
			-- Initialization for `Current'.
		--local
			--dum_graph: LIST_GRAPH[COORDINATE]
			--hey: COORDINATE
			--joke :  VERTEX[COORDINATE]
		do
			create maze_gen.make
			--create dum_graph.make_empty
			create maze_graph.make_empty
			create maze_printout.make_empty
			--create hey.make([1,1])
			--create joke.make (hey)
			--dum_graph.add_vertex (joke)
			create maze_draw.make (maze_gen.generate_new_maze (1))
		end

feature -- model attributes
	i: INTEGER
	game_num: INTEGER
	score: INTEGER
	score_count: INTEGER
	total_count: INTEGER
	maze_gen: MAZE_GENERATOR
	maze_printout: STRING
	maze_graph: LIST_GRAPH[COORDINATE]
	du: DIRECTION_UTILITY
	maze_draw: MAZE_DRAWER
	error_check: INTEGER
	end_game:BOOLEAN
	cur_row: INTEGER
	cur_col: INTEGER
	abort_ed: BOOLEAN
feature -- model operations
	new_game(level: INTEGER_32)
			-- Perform update to the model state.
		do
			create maze_gen.make
			maze_graph:= maze_gen.generate_new_maze (level)
			create maze_draw.make (maze_graph)
			cur_row := maze_draw.player_coord.row -1
			cur_col := maze_draw.player_coord.col
			game_num:= game_num+1;
			i:= i+1
			if level ~ 1 then
				score_count:= 1
				total_count:=total_count+1
			end
			if level ~ 2 then
				score_count:= 2
				total_count:=total_count+2
			end
			if level ~ 3 then
				score_count:= 3
				total_count:=total_count+3
			end
			end_game:=false
			error_check:=1
			abort_ed:= false
		end
	solve
			-- Perform update to the model state.
		do
				
		end
	abort
			-- Perform update to the model state.
		do
			maze_draw.out.wipe_out
			score:=0;
			abort_ed:=True
			end_game:=true
		end
	move(dir: TUPLE[row_mod: INTEGER; col_mod: INTEGER])
			-- Perform update to the model state.
			local
		test_edge: EDGE[COORDINATE]
		test_cord: COORDINATE
		main_cord: COORDINATE

		t: VERTEX[COORDINATE]
		m: VERTEX[COORDINATE]
		do

				create main_cord.make ( [cur_row ,cur_col])
				create test_cord.make ( [cur_row + dir.row_mod,cur_col + dir.col_mod] )
				create t.make (test_cord)
				create m.make (main_cord)
				create test_edge.make (m, t)
				error_check:=0
				across maze_graph.vertices as cursor
					loop
							across cursor.item.outgoing_sorted as l
							loop
								if l.item.source ~ test_edge.source and l.item.destination ~ test_edge.destination
			    				then
			    					maze_draw.move_player (dir)
			    					cur_row := test_cord.row
			    					cur_col := test_cord.col
			    					error_check:=1;
									--maze_printout.append (l.item.out)
									--maze_printout.append ("MOVEMENT POINTS %N")
			    				end
			    			end

			    	end
					--maze_printout.append (test_edge.destination.out)
					--if main_cord1.outgoing_sorted.has (test_edge)
				 	--then
						--error_check:=1
					--else
					--error_check:=0
					--	maze_draw.move_player (dir)
				 --	end

				 if maze_draw.player_coord.row ~ ((maze_draw.size * 2)+1) and maze_draw.player_coord.col ~ maze_draw.size then
				 	end_game:=true
				 	score:= score_count
				 	total_count:=score_count
				 end
				i:=i+1;
		end

	reset
			-- Reset model state.
		do
			make
		end

feature -- queries
	out : STRING
	local
		test_edge: EDGE[COORDINATE]
		test_cord: COORDINATE
		main_cord: COORDINATE

		t: VERTEX[COORDINATE]
		m: VERTEX[COORDINATE]
		do
			create Result.make_from_string (" ")
			--Result.append (maze_printout.out)
			--Result.append ("%N")
			if error_check=0 and abort_ed ~ false then
				Result.append (" State: ")
			    Result.append (i.out)
				Result.append(" -> Error! Not a valid move.%N")
			else if error_check=1 and  abort_ed ~ false then
				Result.append ("  State: ")
				Result.append (i.out)
				Result.append (" -> ok")
				Result.append (maze_draw.out)
			end
			    Result.append("  %N  Game Number: ")
				Result.append (game_num.out)
				Result.append ("%N  Score: ")
				Result.append (score.out)
				Result.append ("/")
				Result.append (total_count.out)
				Result.append ("%N")
			--create main_cord.make ([2,2])
			--create test_cord.make ([3,2])
			----create t.make (test_cord)
			--create m.make(main_cord)
			--create test_edge.make (m, t)

				--across maze_graph.vertices as cursor
					--loop
						--across cursor.item.outgoing_sorted as l
			    			--loop
			    					--if l.item.source ~ test_edge.source --and l.item.destination ~ test_edge.destination
			    					--then
			    						--Result.append ("CHECKING DESTINATION %N")
										--Result.append (l.item.out)
									--end
								--Result.append ("src:")
					   			--Result.append (l_cor.item.source.out)
								--Result.append ("dst:")
								--Result.append (l_cor.item.destination.out)
			    			--end

			    	--end
				if end_game ~ True and score >1 then
					Result.append ("  Congratulations! You escaped the maze!")
				elseif end_game ~ True and score ~ 0 then
					Result.append ("  Game was aborted")
				end








				--Result.append (hey.out)
				--Result.append("[")
				--Result.append(cur_row.out)
				--Result.append(",")
			--Result.append(cur_col.out)
				--Result.append("] ")
				--across maze_draw.added_edges as cursor
				--loop
					--if cursor.item.source ~ [2,2] and cursor.item.destination ~ [4,2]then
					--Result.append ("src:")
					--Result.append (cursor.item.source.out)
					--Result.append ("dst:")
					--Result.append (cursor.item.destination.out)
					--end
				--Result.append (joke.out)
				---Result.append (maze_draw.maze_ascii[4,3].out)

				--end
			end


		end
end




