pragma solidity ^0.4.0;

/**
 * @title SafeMath
 * @dev Math operations with safety checks that throw on error
 */
library SafeMath {
  function mul(uint256 a, uint256 b) internal constant returns (uint256) {
    uint256 c = a * b;
    assert(a == 0 || c / a == b);
    return c;
  }

  function div(uint256 a, uint256 b) internal constant returns (uint256) {
    // assert(b > 0); // Solidity automatically throws when dividing by 0
    uint256 c = a / b;
    // assert(a == b * c + a % b); // There is no case in which this doesn't hold
    return c;
  }

  function sub(uint256 a, uint256 b) internal constant returns (uint256) {
    assert(b <= a);
    return a - b;
  }

  function add(uint256 a, uint256 b) internal constant returns (uint256) {
    uint256 c = a + b;
    assert(c >= a);
    return c;
  }
}

/**
 * @title ERC20Basic
 * @dev Simpler version of ERC20 interface
 * @dev see https://github.com/ethereum/EIPs/issues/179
 */
contract ERC20Basic {
  uint256 public totalSupply;
  function balanceOf(address who) constant returns (uint256);
  function transfer(address to, uint256 value) returns (bool);
  event Transfer(address indexed from, address indexed to, uint256 value);
}

/**
 * @title Basic token
 * @dev Basic version of StandardToken, with no allowances.
 */
contract BasicToken is ERC20Basic {
  using SafeMath for uint256;

  mapping(address => uint256) balances;

  /**
  * @dev transfer token for a specified address
  * @param _to The address to transfer to.
  * @param _value The amount to be transferred.
  */
  function transfer(address _to, uint256 _value) returns (bool) {
    balances[msg.sender] = balances[msg.sender].sub(_value);
    balances[_to] = balances[_to].add(_value);
    Transfer(msg.sender, _to, _value);
    return true;
  }

  /**
  * @dev Gets the balance of the specified address.
  * @param _owner The address to query the the balance of.
  * @return An uint256 representing the amount owned by the passed address.
  */
  function balanceOf(address _owner) constant returns (uint256 balance) {
    return balances[_owner];
  }

}

/**
 * @title ERC20 interface
 * @dev see https://github.com/ethereum/EIPs/issues/20
 */
contract ERC20 is ERC20Basic {
  function allowance(address owner, address spender) constant returns (uint256);
  function transferFrom(address from, address to, uint256 value) returns (bool);
  function approve(address spender, uint256 value) returns (bool);
  event Approval(address indexed owner, address indexed spender, uint256 value);
}

/**
 * @title Standard ERC20 token
 *
 * @dev Implementation of the basic standard token.
 * @dev https://github.com/ethereum/EIPs/issues/20
 * @dev Based on code by FirstBlood: https://github.com/Firstbloodio/token/blob/master/smart_contract/FirstBloodToken.sol
 */
contract StandardToken is ERC20, BasicToken {

  mapping (address => mapping (address => uint256)) allowed;


  /**
   * @dev Transfer tokens from one address to another
   * @param _from address The address which you want to send tokens from
   * @param _to address The address which you want to transfer to
   * @param _value uint256 the amout of tokens to be transfered
   */
  function transferFrom(address _from, address _to, uint256 _value) returns (bool) {
    var _allowance = allowed[_from][msg.sender];

    // Check is not needed because sub(_allowance, _value) will already throw if this condition is not met
    // require (_value <= _allowance);

    balances[_to] = balances[_to].add(_value);
    balances[_from] = balances[_from].sub(_value);
    allowed[_from][msg.sender] = _allowance.sub(_value);
    Transfer(_from, _to, _value);
    return true;
  }

  /**
   * @dev Aprove the passed address to spend the specified amount of tokens on behalf of msg.sender.
   * @param _spender The address which will spend the funds.
   * @param _value The amount of tokens to be spent.
   */
  function approve(address _spender, uint256 _value) returns (bool) {

    // To change the approve amount you first have to reduce the addresses`
    //  allowance to zero by calling `approve(_spender, 0)` if it is not
    //  already 0 to mitigate the race condition described here:
    //  https://github.com/ethereum/EIPs/issues/20#issuecomment-263524729
    require((_value == 0) || (allowed[msg.sender][_spender] == 0));

    allowed[msg.sender][_spender] = _value;
    Approval(msg.sender, _spender, _value);
    return true;
  }

  /**
   * @dev Function to check the amount of tokens that an owner allowed to a spender.
   * @param _owner address The address which owns the funds.
   * @param _spender address The address which will spend the funds.
   * @return A uint256 specifing the amount of tokens still available for the spender.
   */
  function allowance(address _owner, address _spender) constant returns (uint256 remaining) {
    return allowed[_owner][_spender];
  }

}

/**
 * @title SimpleToken
 * @author : "Srihari Kapu"
 * @dev Very simple ERC20 Token example, where all tokens are pre-assigned to the creator.
 * Note they can later distribute these tokens as they wish using `transfer` and other
 * `StandardToken` functions.
 */
 
/*************** Ownable
***/

contract Ownable {
  address public owner;

  function Ownable() {
    owner = msg.sender;
  }

  modifier onlyOwner() {
    require(msg.sender == owner);
    
    _;
  }

  function transferOwnership(address newOwner) onlyOwner {
    if (newOwner != address(0)) {
      owner = newOwner;
    }
  }

}


/**
 * @title SimpleToken
 * @author : "Srihari Kapu"
 * @dev Very simple ERC20 Token example, where all tokens are pre-assigned to the creator.
 * Note they can later distribute these tokens as they wish using `transfer` and other
 * `StandardToken` functions.
 */
contract AssetBasedToken is Ownable, SafeMath, ERC20, Pausable {

  event Transfer(address indexed from, address indexed to, uint value);
  event Approval(address indexed owner, address indexed spender, uint value);
  event DeductFees(address indexed owner,uint256 amount);

  event TokenMinted(address destination, uint256 amount);
  event TokenBurned(address source, uint256 amount);
  
	string public name = "AssetBasedToken";
	string public symbol = "AST";
	uint256 constant public  decimals = 18;  // same as ETH
	uint256 constant public  ASETDecimals = 8;
		
	uint256 constant public allocationPool = 1 *  10**9 * 10**ASETDecimals;      // total ASET holdings
	uint256	constant public	maxAllocation  = 38 * 10**5 * 10**decimals;			// max AST that can ever ever be given out
	uint256	         public	totAllocation;			// amount of AST so far
	
	address			 public feeCalculator;
	address		     public ASET;					// ASET contract address



	function setFeeCalculator(address newFC) onlyOwner {
		feeCalculator = newFC;
	}


	function calcFees(uint256 from, uint256 to, uint256 amount) returns (uint256 val, uint256 fee) {
		return gasFees(feeCalculator).calcFees(from,to,amount);
	}

	function AssetBasedToken(address feeCalc) {
		feeCalculator = feeCalc;
	}

    struct allocation { 
        uint256     amount;
        uint256     date;
    }
	
	allocation[]   public allocationsOverTime;
	allocation[]   public currentAllocations;

	function currentAllocationLength() constant returns (uint256) {
		return currentAllocations.length;
	}

	function aotLength() constant returns (uint256) {
		return allocationsOverTime.length;
	}

	
    struct Balance {
        uint256 amount;                 // amount through update or transfer
        uint256 lastUpdated;            // DATE last updated
        uint256 nextAllocationIndex;    // which allocationsOverTime record contains next update
        uint256 allocationShare;        // the share of allocationPool that this holder gets (means they hold ASET)
    }

	/*Creates an array with all balances*/
	mapping (address => Balance) public balances;
	mapping (address => mapping (address => uint)) allowed;
	
	function update(address where) internal {
        uint256 pos;
		uint256 fees;
		uint256 val;
        (val,fees,pos) = updatedBalance(where);
	    balances[where].nextAllocationIndex = pos;
	    balances[where].amount = val;
        balances[where].lastUpdated = now;
	}
	
	function updatedBalance(address where) constant public returns (uint val, uint fees, uint pos) {
		uint256 c_val;
		uint256 c_fees;
		uint256 c_amount;

		(val, fees) = calcFees(balances[where].lastUpdated,now,balances[where].amount);

	    pos = balances[where].nextAllocationIndex;
		if ((pos < currentAllocations.length) &&  (balances[where].allocationShare != 0)) {

			c_amount = currentAllocations[balances[where].nextAllocationIndex].amount * balances[where].allocationShare / allocationPool;

			(c_val,c_fees)   = calcFees(currentAllocations[balances[where].nextAllocationIndex].date,now,c_amount);

		} 

	    val  += c_val;
		fees += c_fees;
		pos   = currentAllocations.length;
	}

    function balanceOf(address where) constant returns (uint256 val) {
        uint256 fees;
		uint256 pos;
        (val,fees,pos) = updatedBalance(where);
        return ;
    }

	event Allocation(uint256 amount, uint256 date);
	event FeeOnAllocation(uint256 fees, uint256 date);

	event PartComplete();
	event StillToGo(uint numLeft);
	uint256 public partPos;
	uint256 public partFees;
	uint256 partL;
	allocation[]   public partAllocations;

	function partAllocationLength() constant returns (uint) {
		return partAllocations.length;
	}

	function addAllocationPartOne(uint newAllocation,uint numSteps) onlyOwner{
		uint256 thisAllocation = newAllocation;

		require(totAllocation < maxAllocation);		// cannot allocate more than this;

		if (currentAllocations.length > partAllocations.length) {
			partAllocations = currentAllocations;
		}

		if (totAllocation + thisAllocation > maxAllocation) {
			thisAllocation = maxAllocation - totAllocation;
			log0("max alloc reached");
		}
		totAllocation += thisAllocation;

		Allocation(thisAllocation,now);

        allocation memory newDiv;
        newDiv.amount = thisAllocation;
        newDiv.date = now;
		// store into history
	    allocationsOverTime.push(newDiv);
		// add this record to the end of currentAllocations
		partL = partAllocations.push(newDiv);
		// update all other records with calcs from last record
		if (partAllocations.length < 2) { // no fees to consider
			PartComplete();
			currentAllocations = partAllocations;
			FeeOnAllocation(0,now);
			return;
		}
		//
		// The only fees that need to be collected are the fees on location zero.
		// Since they are the last calculated = they come out with the break
		//
		for (partPos = partAllocations.length - 2; partPos >= 0; partPos-- ){
			(partAllocations[partPos].amount,partFees) = calcFees(partAllocations[partPos].date,now,partAllocations[partPos].amount);

			partAllocations[partPos].amount += partAllocations[partL - 1].amount;
			partAllocations[partPos].date    = now;
			if ((partPos == 0) || (partPos == partAllocations.length-numSteps)){
				break; 
			}
		}
		if (partPos != 0) {
			StillToGo(partPos);
			return; // not done yet
		}
		PartComplete();
		FeeOnAllocation(partFees,now);
		currentAllocations = partAllocations;
	}

	function addAllocationPartTwo(uint numSteps) onlyOwner {
		require(numSteps > 0);
		require(partPos > 0);
		for (uint i = 0; i < numSteps; i++ ){
			partPos--;
			(partAllocations[partPos].amount,partFees) = calcFees(partAllocations[partPos].date,now,partAllocations[partPos].amount);

			partAllocations[partPos].amount += partAllocations[partL - 1].amount;
			partAllocations[partPos].date    = now;
			if (partPos == 0) {
				break; 
			}
		}
		if (partPos != 0) {
			StillToGo(partPos);
			return; // not done yet
		}
		PartComplete();
		FeeOnAllocation(partFees,now);
		currentAllocations = partAllocations;
	}


	function setASET(address _ASET) onlyOwner {
		ASET = _ASET;
	}

	function parentFees(address where) whenNotPaused {
		require(msg.sender == ASET);
	    update(where);		
	}
	
	function parentChange(address where, uint newValue) whenNotPaused { // called when ASET balance changes
		require(msg.sender == ASET);
	    balances[where].allocationShare = newValue;
	}
	
	/* send AST */
	function transfer(address _to, uint256 _value) whenNotPaused returns (bool ok) {
	    update(msg.sender);              // Do this to ensure sender has enough funds.
		update(_to); 

        balances[msg.sender].amount = safeSub(balances[msg.sender].amount, _value);
        balances[_to].amount = safeAdd(balances[_to].amount, _value);

		Transfer(msg.sender, _to, _value); //Notify anyone listening that this transfer took place
        return true;
	}

	function transferFrom(address _from, address _to, uint _value) whenNotPaused returns (bool success) {
		var _allowance = allowed[_from][msg.sender];

	    update(_from);              // Do this to ensure sender has enough funds.
		update(_to); 

		balances[_to].amount = safeAdd(balances[_to].amount, _value);
		balances[_from].amount = safeSub(balances[_from].amount, _value);
		allowed[_from][msg.sender] = safeSub(_allowance, _value);
		Transfer(_from, _to, _value);
		return true;
	}

  	function approve(address _spender, uint _value) whenNotPaused returns (bool success) {
		require((_value == 0) || (allowed[msg.sender][_spender] == 0));
    	allowed[msg.sender][_spender] = _value;
    	Approval(msg.sender, _spender, _value);
    	return true;
  	}

  	function allowance(address _owner, address _spender) constant returns (uint remaining) {
    	return allowed[_owner][_spender];
  	}

	// Minting Functions 
	address public authorisedMinter;

	function setMinter(address minter) onlyOwner {
		authorisedMinter = minter;
	}
	
	function mintTokens(address destination, uint256 amount) {
		require(msg.sender == authorisedMinter);
		update(destination);
		balances[destination].amount = safeAdd(balances[destination].amount, amount);
		balances[destination].lastUpdated = now;
		balances[destination].nextAllocationIndex = currentAllocations.length;
		TokenMinted(destination,amount);
	}

	function burnTokens(address source, uint256 amount) {
		require(msg.sender == authorisedMinter);
		update(source);
		balances[source].amount = safeSub(balances[source].amount,amount);
		balances[source].lastUpdated = now;
		balances[source].nextAllocationIndex = currentAllocations.length;
		TokenBurned(source,amount);
	}
}
