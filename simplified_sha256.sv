module simplified_sha256 #(parameter integer NUM_OF_WORDS = 40)(
 input logic  clk, rst_n, start,
 input logic  [15:0] input_addr, hash_addr,
 output logic done, memory_clk, enable_write,
 output logic [15:0] memory_addr,
 output logic [31:0] memory_write_data,
 input logic [31:0] memory_read_data);

// FSM state variables 
enum logic [2:0] {IDLE, BLOCK, COMPUTE, WRITE, READ, WAIT, INIT_WRITE} state;

parameter integer SIZE = NUM_OF_WORDS * 32;  

// NOTE : Below mentioned frame work is for reference purpose.
// Local variables might not be complete and you might have to add more variables
// or modify these variables. Code below is more as a reference.

// Local variables
logic [31:0] w[16];
logic [31:0] hash_out[8];
logic [31:0] message[16];
logic [31:0] wt;
logic [31:0] S0,S1;
logic [31:0] hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7;
logic [31:0] A, B, C, D, E, F, G, H;
logic [ 7:0] i, j;
logic block_done;
logic compute_done;
logic [15:0] offset; // in word address
logic [ 7:0] num_blocks;
logic [7:0] word_count;
logic [15:0] present_addr;
logic [31:0] present_write_data;
logic [63:0] message_length;
logic [512:0] data_read;
logic [ 7:0] tstep;
logic [31:0] s0, s1;



// SHA256 K constants
parameter int k[0:63] = '{
   32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
   32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
   32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
   32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
   32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
   32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
   32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
   32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};


// Generate request to memory
// for reading from memory to get original message
// for writing final computed has value
assign memory_clk = clk;
assign memory_addr = present_addr + offset;
assign memory_we = enable_write;
assign memory_write_data = present_write_data;


assign num_blocks = determine_num_blocks(NUM_OF_WORDS); 
assign message_length = NUM_OF_WORDS << 5;
assign tstep = (i - 1);

// Note : Function defined are for reference purpose. Feel free to add more functions or modify below.
// Function to determine number of blocks in memory to fetch
function logic [15:0] determine_num_blocks(input logic [31:0] size);

  // Student to add function implementation
	determine_num_blocks = (size + 18) >> 4;
 
endfunction


// SHA256 hash round
function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                 input logic [7:0] t);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
begin
    S1 = ror(e, 6) ^ ror(e, 11) ^ ror(e, 25);
    ch = (e & f) ^ ((~e) & g);
    t1 = h + S1 + ch + k[t] + w;
    S0 = ror(a, 2) ^ ror(a, 13) ^ ror(a, 22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    t2 = S0 + maj;

    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};

end
endfunction

// Right Rotation Example : right rotate input x by r
// Lets say input x = 1111 ffff 2222 3333 4444 6666 7777 8888
// lets say r = 4
// x >> r  will result in : 0000 1111 ffff 2222 3333 4444 6666 7777 
// x << (32-r) will result in : 8888 0000 0000 0000 0000 0000 0000 0000
// final right rotate expression is = (x >> r) | (x << (32-r));
// (0000 1111 ffff 2222 3333 4444 6666 7777) | (8888 0000 0000 0000 0000 0000 0000 0000)
// final value after right rotate = 8888 1111 ffff 2222 3333 4444 6666 7777
// Right rotation function

function logic [31:0] ror(input logic [31:0] in,
                                  input logic [7:0] s);
begin
		ror = (in >> s) | (in << (32-s));
end
endfunction

function logic [31:0] wtnew; // function with no inputs
  logic [31:0] s0, s1;
  s0 = ror(w[1],7)^ror(w[1],18)^(w[1]>>3);
  s1 = ror(w[14],17)^ror(w[14],19)^(w[14]>>10);
  wtnew = w[0] + s0 + w[9] + s1;
endfunction



// SHA-256 FSM 
// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function
// and write back hash value back to memory
always_ff @(posedge clk, negedge rst_n) begin
  if (!rst_n) begin
		state <= IDLE;
		enable_write <= 0;
  end
  else begin 
	  case (state)
		// Initialize hash values h0 to h7 and a to h, other variables and memory we, address offset, etc
		IDLE: begin 
			if(start) begin 
				
				 hash0 <= 32'h6a09e667;
				 hash1 <= 32'hbb67ae85;
				 hash2 <= 32'h3c6ef372;
				 hash3 <= 32'ha54ff53a;
				 hash4 <= 32'h510e527f;
				 hash5 <= 32'h9b05688c;
				 hash6 <= 32'h1f83d9ab;
				 hash7 <= 32'h5be0cd19;
				 
				 A <= 32'h6a09e667;
				 B <= 32'hbb67ae85;
				 C <= 32'h3c6ef372;
				 D <= 32'ha54ff53a;
				 E <= 32'h510e527f;
				 F <= 32'h9b05688c;
				 G <= 32'h1f83d9ab;
				 H <= 32'h5be0cd19;
				 i <= 0;
				 j <= 0;
				 state <= BLOCK;
				 present_addr <= input_addr;
				 offset <= 0;
				 enable_write <= 0;
				 block_done<=0;
				 word_count<=0;
				 compute_done<=0;
		   end
		end

		READ: begin
			 if((word_count > NUM_OF_WORDS) & (word_count % 16 == 14)) begin
					message[offset] <= message_length[63:32];
					message[offset + 1] <= message_length[31:0];
					state <=WAIT;  
					word_count <= word_count + 2;
					
					block_done<=1;
					offset<=0;
					
			 end else if(word_count > NUM_OF_WORDS) begin
				
				message[offset] <= 32'd0;
				state <=WAIT;  
				word_count <= word_count + 1;
				
				if(offset<=15) begin
				  offset<=offset+1; 
				  block_done<=0;
				end else begin
				  block_done<=1;
				  offset<=0;
				end
			end if(word_count == NUM_OF_WORDS) begin
				message[offset]<= 32'b10000000000000000000000000000000;
				state <=WAIT;  
				word_count <= word_count + 1;
				if(offset<=15) begin
				  offset<=offset+1; 
				  block_done<=0;
				end else begin
				  block_done<=1;
				  offset<=0;
				end
			end else if (word_count < NUM_OF_WORDS) begin
				message[offset]<=memory_read_data;
				state <=WAIT;  
				word_count <= word_count + 1;
				if(offset<=15) begin
				  offset<=offset+1; 
				  block_done<=0;
				end else begin
				  block_done<=1;
				  offset<=0;
				end
			end
		 end
		
		WAIT: begin 
			if(block_done == 1) begin
				word_count <= word_count -1;
				state <= BLOCK;
			end else begin
				state <= READ;
			end
		end
		
		// SHA-256 FSM 
		// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function    
		// and write back hash value back to memory
		BLOCK: begin
		// Fetch message in 512-bit block size
		// For each of 512-bit block initiate hash value computation
			if((i == num_blocks) && (compute_done == 1)) begin
				state <= INIT_WRITE;
			end else if(block_done == 0) begin // reading not done
			   present_addr <= input_addr + 16 * i;
				i <= i + 1;
				state <= WAIT;
				compute_done <= 0;
			end else begin
				//reading is done
				block_done <= 0;
				compute_done <= 0;
				for(int t = 0; t < 16; t++) begin
						w[t] <= message[t];
				end 
				state <= COMPUTE;
				
			end
			
			

		end
		
		// For each block compute hash function
		// Go back to BLOCK stage after each block hash computation is completed and if
		// there are still number of message blocks available in memory otherwise
		// move to WRITE stage
		COMPUTE: begin
		// 64 processing rounds steps for 512-bit block 
			if(j < 64) begin 
				{A, B, C, D, E, F, G, H} <= sha256_op(A,B,C,D,E,F,G,H,w[0],j);
				j <= j + 1;
				for (int n = 0; n < 15; n++) begin
					w[n] <= w[n+1]; 
				end
				
				w[15] <= wtnew();

				state<=COMPUTE;
			end else begin
				j <= 0;
			    hash0 <= hash0 + A;
				 hash1 <= hash1 + B;
				 hash2 <= hash2 + C;
				 hash3 <= hash3 + D;
				 hash4 <= hash4 + E;
				 hash5 <= hash5 + F;
				 hash6 <= hash6 + G;
				 hash7 <= hash7 + H;
				 A <= hash0 + A;
				 B <= hash1 + B;
				 C <= hash2 + C;
				 D <= hash3 + D;
				 E <= hash4 + E;
				 F <= hash5 + F;
				 G <= hash6 + G;
				 H <= hash7 + H;
				state <= BLOCK;
				compute_done <=1;
			end
					
		end
		
		INIT_WRITE: begin
			hash_out[0] <= hash0;
			hash_out[1] <= hash1;
			hash_out[2] <= hash2;
			hash_out[3] <= hash3;
			hash_out[4] <= hash4;
			hash_out[5] <= hash5;
			hash_out[6] <= hash6;
			hash_out[7] <= hash7;
			present_addr <= hash_addr - 1;
			offset <= 0;
			state <= WRITE;
		end
		// h0 to h7 each are 32 bit hashes, which makes up total 256 bit value
		// h0 to h7 after compute stage has final computed hash value
		// write back these h0 to h7 to memory starting from output_addr
		WRITE: begin
			if(offset < 8) begin
				present_write_data <= hash_out[offset];
				enable_write <= 1;
				offset <= offset + 1;
				state <= WRITE;
			end else begin
				state <= IDLE;
				offset <= 0;
				enable_write <= 0;
			end
		end
      endcase
	end
end

// Generate done when SHA256 hash computation has finished and moved to IDLE state
assign done = (state == IDLE);

endmodule
