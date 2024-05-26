
module picorv32 (
	input	   clk, resetn, mem_ready, mem_la_read, mem_la_write, mem_la_addr, mem_la_wstrb, mem_la_wdata, mem_do_prefetch, mem_do_rinst, mem_do_rdata, mem_la_use_prefetched_high_word, clear_prefetched_high_word,
	output reg trap, mem_state, mem_valid, mem_la_secondword, prefetched_high_word, mem_addr, mem_wstrb, mem_wdata, mem_instr);

	always @(posedge clk) begin
		if (!resetn || trap) begin
			if (!resetn)
				mem_state <= 0;
			if (!resetn || mem_ready)
				mem_valid <= 0;
			mem_la_secondword <= 0;
			prefetched_high_word <= 0;
		end else begin
			if (mem_la_read || mem_la_write) begin
				mem_addr <= mem_la_addr;
				mem_wstrb <= mem_la_wstrb & {4{mem_la_write}};
			end
			if (mem_la_write) begin
				mem_wdata <= mem_la_wdata;
			end
			case (mem_state)
				0: begin
					if (mem_do_prefetch || mem_do_rinst || mem_do_rdata) begin
						mem_valid <= !mem_la_use_prefetched_high_word;
						mem_instr <= mem_do_prefetch || mem_do_rinst;
						mem_wstrb <= 0;
						mem_state <= 1;
					end
				end
			endcase
		end

		if (clear_prefetched_high_word)
			prefetched_high_word <= 0;
	end
endmodule
