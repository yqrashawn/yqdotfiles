require 'rexml/document'

module Alfred
  class Workflow
    def feedback_items
      @feedback_items ||= []
    end

    def feedback_xml
      doc = REXML::Document.new
      doc << REXML::Element.new('items')
      feedback_items
      .sort { |a,b| b.priority <=> a.priority}
      .each { |item| doc.root << item.to_xml }

      doc
    end

    def send_feedback!
      puts feedback_xml.to_s
    end
  end
end

module Alfred
  module Feedback
    class Item
      ITEM_ATTRIBUTES = [:uid, :arg, :autocomplete, :type, :valid]
      ITEM_ATTRIBUTES.each { |attribute| attr_accessor attribute }

      attr_accessor :priority

      def initialize
        @subelements = []
        @priority = 0
      end

      def valid=(valid)
        @valid = valid ? :yes : :no
      end

      def add_title(title, **args)
        add_subelement('title', title, **args)
      end

      def add_subtitle(subtitle, **args)
        add_subelement('subtitle', subtitle, **args)
      end

      def add_icon(icon, **args)
        add_subelement('icon', icon, **args)
      end

      def add_text(text, **args)
        add_subelement('text', text, **args)
      end

      def to_xml
        item = REXML::Element.new('item')
        ITEM_ATTRIBUTES.each do |attr|
          item.add_attribute(attr.to_s, send(attr).to_s) unless send(attr).nil?
        end

        @subelements.each do |hash|
          element = REXML::Element.new(hash[:name])
          element.add_attributes(hash[:attributes])
          element.text = hash[:text]
          item.add_element(element)
        end

        item
      end

      private

      def add_subelement(element_name, text, **args)
        # REXML requires attributes be String objects
        args_s = {}
        args.each_pair { |k,v| args_s[k.to_s] = v.to_s }

        @subelements << {name: element_name, attributes: args_s, text: text.to_s}
      end
    end
  end
end
