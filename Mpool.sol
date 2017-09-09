pragma solidity ^0.4.8;


contract SafeMath {

  function assert(bool assertion) internal {
    if (!assertion) return;
  }

  function safeMul(uint a, uint b) internal returns (uint) {
    uint c = a * b;
    assert(a == 0 || c / a == b);
    return c;
  }

  function safeDiv(uint a, uint b) internal returns (uint) {
    assert(b > 0);
    uint c = a / b;
    assert(a == b * c + a % b);
    return c;
  }

}


contract StandardTokenProtocol {

  
    event Transfer(address indexed _from, address indexed _recipient, uint256 _value);
    event Approval(address indexed _owner, address indexed _spender, uint256 _value);

}


contract StandardToken is StandardTokenProtocol {

    modifier when_can_transfer(address _from, uint256 _value) {
        if (balances[_from] >= _value) _;
    }

    modifier when_can_receive(address _recipient, uint256 _value) {
        if (balances[_recipient] + _value > balances[_recipient]) _;
    }

    modifier when_is_allowed(address _from, address _delegate, uint256 _value) {
        if (allowed[_from][_delegate] >= _value) _;
    }

    function transfer(address _recipient, uint256 _value)
        when_can_transfer(msg.sender, _value)
        when_can_receive(_recipient, _value)
        returns (bool o_success)
    {
        balances[msg.sender] -= _value;
        balances[_recipient] += _value;
        Transfer(msg.sender, _recipient, _value);
        return true;
    }

    function transferFrom(address _from, address _recipient, uint256 _value)
        when_can_transfer(_from, _value)
        when_can_receive(_recipient, _value)
        when_is_allowed(_from, msg.sender, _value)
        returns (bool o_success)
    {
        allowed[_from][msg.sender] -= _value;
        balances[_from] -= _value;
        balances[_recipient] += _value;
        Transfer(_from, _recipient, _value);
        return true;
    }

    function balanceOf(address _owner) constant returns (uint256 balance) {
        return balances[_owner];
    }

    function approve(address _spender, uint256 _value) returns (bool o_success) {
        allowed[msg.sender][_spender] = _value;
        Approval(msg.sender, _spender, _value);
        return true;
    }

    function allowance(address _owner, address _spender) constant returns (uint256 o_remaining) {
        return allowed[_owner][_spender];
    }

    mapping (address => uint256) balances;
    mapping (address => mapping (address => uint256)) allowed;
    uint256 public totalSupply;

}

contract GUPToken is StandardToken {

	//FIELDS
	string public name = "Guppy";
    string public symbol = "GUP";
    uint public decimals = 3;

	//CONSTANTS
	uint public constant LOCKOUT_PERIOD = 1 years; //time after end date that illiquid GUP can be transferred

	//ASSIGNED IN INITIALIZATION
	uint public endMintingTime; //Timestamp after which no more tokens can be created
	address public minter; //address of the account which may mint new tokens

	mapping (address => uint) public illiquidBalance; //Balance of 'Frozen funds'

	//MODIFIERS
	//Can only be called by contribution contract.
	modifier only_minter {
		if (msg.sender != minter) return;
		_;
	}

	// Can only be called if illiquid tokens may be transformed into liquid.
	// This happens when `LOCKOUT_PERIOD` of time passes after `endMintingTime`.
	modifier when_thawable {
		if (now < endMintingTime + LOCKOUT_PERIOD) return;
		_;
	}

	// Can only be called if (liquid) tokens may be transferred. Happens
	// immediately after `endMintingTime`.
	modifier when_transferable {
		if (now < endMintingTime) return;
		_;
	}

	// Can only be called if the `crowdfunder` is allowed to mint tokens. Any
	// time before `endMintingTime`.
	modifier when_mintable {
		if (now >= endMintingTime) return;
		_;
	}

	// Initialization contract assigns address of crowdfund contract and end time.
	function GUPToken(address _minter, uint _endMintingTime) {
		endMintingTime = _endMintingTime;
		minter = _minter;
	}

	// Create new tokens when called by the crowdfund contract.
	// Only callable before the end time.
	function createToken(address _recipient, uint _value)
		when_mintable
		only_minter
		returns (bool o_success)
	{
		balances[_recipient] += _value;
		totalSupply += _value;
		return true;
	}

	// Create an illiquidBalance which cannot be traded until end of lockout period.
	// Can only be called by crowdfund contract before the end time.
	function createIlliquidToken(address _recipient, uint _value)
		when_mintable
		only_minter
		returns (bool o_success)
	{
		illiquidBalance[_recipient] += _value;
		totalSupply += _value;
		return true;
	}

	// Make sender's illiquid balance liquid when called after lockout period.
	function makeLiquid()
		when_thawable
	{
		balances[msg.sender] += illiquidBalance[msg.sender];
		illiquidBalance[msg.sender] = 0;
	}

	// Transfer amount of tokens from sender account to recipient.
	// Only callable after the crowd fund end date.
	function transfer(address _recipient, uint _amount)
		when_transferable
		returns (bool o_success)
	{
		return super.transfer(_recipient, _amount);
	}

	// Transfer amount of tokens from a specified address to a recipient.
	// Only callable after the crowd fund end date.
	function transferFrom(address _from, address _recipient, uint _amount)
		when_transferable
		returns (bool o_success)
	{
		return super.transferFrom(_from, _recipient, _amount);
	}
}


contract Contribution is SafeMath {

	//FIELDS

	//CONSTANTS
	//Time limits
	uint public constant STAGE_ONE_TIME_END = 5 hours;
	uint public constant STAGE_TWO_TIME_END = 72 hours;
	uint public constant STAGE_THREE_TIME_END = 2 weeks;
	uint public constant STAGE_FOUR_TIME_END = 4 weeks;
	//Prices of GUP
	uint public constant PRICE_STAGE_ONE   = 480000;
	uint public constant PRICE_STAGE_TWO   = 440000;
	uint public constant PRICE_STAGE_THREE = 400000;
	uint public constant PRICE_STAGE_FOUR  = 360000;
	uint public constant PRICE_BTCS        = 480000;
	//GUP Token Limits
	uint public constant MAX_SUPPLY =        100000000000;
	uint public constant ALLOC_ILLIQUID_TEAM = 8000000000;
	uint public constant ALLOC_LIQUID_TEAM =  13000000000;
	uint public constant ALLOC_BOUNTIES =      2000000000;
	uint public constant ALLOC_NEW_USERS =    17000000000;
	uint public constant ALLOC_CROWDSALE =    60000000000;
	uint public constant BTCS_PORTION_MAX = 31250 * PRICE_BTCS;
	//ASSIGNED IN INITIALIZATION
	//Start and end times
	uint public publicStartTime; //Time in seconds public crowd fund starts.
	uint public privateStartTime; //Time in seconds when BTCSuisse can purchase up to 31250 ETH worth of GUP;
	uint public publicEndTime; //Time in seconds crowdsale ends
	//Special Addresses
	address public btcsAddress; //Address used by BTCSuisse
	address public multisigAddress; //Address to which all ether flows.
	address public matchpoolAddress; //Address to which ALLOC_BOUNTIES, ALLOC_LIQUID_TEAM, ALLOC_NEW_USERS, ALLOC_ILLIQUID_TEAM is sent to.
	address public ownerAddress; //Address of the contract owner. Can halt the crowdsale.
	//Contracts
	GUPToken public gupToken; //External token contract hollding the GUP
	//Running totals
	uint public etherRaised; //Total Ether raised.
	uint public gupSold; //Total GUP created
	uint public btcsPortionTotal; //Total of Tokens purchased by BTC Suisse. Not to exceed BTCS_PORTION_MAX.
	//booleans
	bool public halted; //halts the crowd sale if true.

	//FUNCTION MODIFIERS

	//Is currently in the period after the private start time and before the public start time.
	modifier is_pre_crowdfund_period() {
		if (now >= publicStartTime || now < privateStartTime) return;
		_;
	}

	//Is currently the crowdfund period
	modifier is_crowdfund_period() {
		if (now < publicStartTime || now >= publicEndTime) return;
		_;
	}

	//May only be called by BTC Suisse
	modifier only_btcs() {
		if (msg.sender != btcsAddress) return;
		_;
	}

	//May only be called by the owner address
	modifier only_owner() {
		if (msg.sender != ownerAddress) return;
		_;
	}

	//May only be called if the crowdfund has not been halted
	modifier is_not_halted() {
		if (halted) return;
		_;
	}

	// EVENTS

	event PreBuy(uint _amount);
	event Buy(address indexed _recipient, uint _amount);


	// FUNCTIONS

	//Initialization function. Deploys GUPToken contract assigns values, to all remaining fields, creates first entitlements in the GUP Token contract.
	function Contribution(
		address _btcs,
		address _multisig,
		address _matchpool,
		uint _publicStartTime,
		uint _privateStartTime
	) {
		ownerAddress = msg.sender;
		publicStartTime = _publicStartTime;
		privateStartTime = _privateStartTime;
		publicEndTime = _publicStartTime + 4 weeks;
		btcsAddress = _btcs;
		multisigAddress = _multisig;
		matchpoolAddress = _matchpool;
		gupToken = new GUPToken(this, publicEndTime);
		gupToken.createIlliquidToken(matchpoolAddress, ALLOC_ILLIQUID_TEAM);
		gupToken.createToken(matchpoolAddress, ALLOC_BOUNTIES);
		gupToken.createToken(matchpoolAddress, ALLOC_LIQUID_TEAM);
		gupToken.createToken(matchpoolAddress, ALLOC_NEW_USERS);
	}

	//May be used by owner of contract to halt crowdsale and no longer except ether.
	function toggleHalt(bool _halted)
		only_owner
	{
		halted = _halted;
	}

	//constant function returns the current GUP price.
	function getPriceRate()
		constant
		returns (uint o_rate)
	{
		if (now <= publicStartTime + STAGE_ONE_TIME_END) return PRICE_STAGE_ONE;
		if (now <= publicStartTime + STAGE_TWO_TIME_END) return PRICE_STAGE_TWO;
		if (now <= publicStartTime + STAGE_THREE_TIME_END) return PRICE_STAGE_THREE;
		if (now <= publicStartTime + STAGE_FOUR_TIME_END) return PRICE_STAGE_FOUR;
		else return 0;
	}

	// Given the rate of a purchase and the remaining tokens in this tranche, it
	// will throw if the sale would take it past the limit of the tranche.
	// It executes the purchase for the appropriate amount of tokens, which
	// involves adding it to the total, minting GUP tokens and stashing the
	// ether.
	// Returns `amount` in scope as the number of GUP tokens that it will
	// purchase.
	function processPurchase(uint _rate, uint _remaining)
		internal
		returns (uint o_amount)
	{
		o_amount = safeDiv(safeMul(msg.value, _rate), 1 ether);
		if (o_amount > _remaining) return;
		if (!multisigAddress.send(msg.value)) return;
		if (!gupToken.createToken(msg.sender, o_amount)) return;
		gupSold += o_amount;
		etherRaised += msg.value;
	}

	//Special Function can only be called by BTC Suisse and only during the pre-crowdsale period.
	//Allows the purchase of up to 125000 Ether worth of GUP Tokens.
	function preBuy()
		payable
		is_pre_crowdfund_period
		only_btcs
		is_not_halted
	{
		uint amount = processPurchase(PRICE_BTCS, BTCS_PORTION_MAX - btcsPortionTotal);
		btcsPortionTotal += amount;
		PreBuy(amount);
	}

	//Default function called by sending Ether to this address with no arguments.
	//Results in creation of new GUP Tokens if transaction would not exceed hard limit of GUP Token.
	function()
		payable
		is_crowdfund_period
		is_not_halted
	{
		uint amount = processPurchase(getPriceRate(), ALLOC_CROWDSALE - gupSold);
		Buy(msg.sender, amount);
	}

	//failsafe drain
	function drain()
		only_owner
	{
		if (!ownerAddress.send(this.balance)) return;
	}
}
