<?php
class Workflows {
	private $results;
	function __construct( $bundleid=null ) { $this->results = array(); }
	public function results() { return $this->results; }
	public function toxml( $a=null  ) {
		if ( is_null( $a ) && !empty( $this->results ) ):
			$a = $this->results;
		elseif ( is_null( $a ) && empty( $this->results ) ):
			return false;
		endif;
		$items = new SimpleXMLElement("<items></items>");
		foreach( $a as $b ):
			$c = $items->addChild( 'item' );
			$c_keys = array_keys( $b );
			foreach( $c_keys as $key ):
				if ( $key == 'uid' ):
					$c->addAttribute( 'uid', $b[$key] );
				elseif ( $key == 'arg' ):
					$c->addAttribute( 'arg', $b[$key] );
				elseif ( $key == 'valid' ):
					$c->addAttribute( 'valid', $b[$key] );
				elseif ( $key == 'autocomplete' ):
					$c->addAttribute( 'autocomplete', $b[$key] );
				else:
					$c->$key = $b[$key];
				endif;
			endforeach;
		endforeach;
		return $items->asXML();
	}
	public function result( $uid, $arg, $title, $sub, $icon, $valid='yes', $auto=null )
	{
		if ( is_null( $auto ) ):
			$auto = $title;
		endif;
		$temp = array(
			'uid' => $uid,
			'arg' => $arg,
			'title' => $title,
			'subtitle' => $sub,
			'icon' => $icon,
			'valid' => $valid,
			'autocomplete' => $auto
		);
		array_push( $this->results, $temp );
		return $temp;
	}
	public function request( $url=null, $options=null )
	{
		if ( is_null( $url ) ):
			return false;
		endif;

		$defaults = array(									// Create a list of default curl options
			CURLOPT_RETURNTRANSFER => true,					// Returns the result as a string
			CURLOPT_URL => $url,							// Sets the url to request
			CURLOPT_FRESH_CONNECT => true
		);

		if ( $options ):
			foreach( $options as $k => $v ):
				$defaults[$k] = $v;
			endforeach;
		endif;

		$ch  = curl_init();									// Init new curl object
		curl_setopt_array( $ch, $defaults );				// Set curl options
		$out = curl_exec( $ch );							// Request remote data
		$err = curl_error( $ch );
		curl_close( $ch );									// End curl request

		if ( $err ):
			return $err;
		else:
			return $out;
		endif;
	}
}