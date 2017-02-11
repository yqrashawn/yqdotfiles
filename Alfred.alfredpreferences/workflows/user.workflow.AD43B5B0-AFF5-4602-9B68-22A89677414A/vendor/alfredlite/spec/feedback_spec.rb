require_relative 'spec_helper'

describe Alfred::Feedback::Item do
  before(:each) do
    @item = described_class.new
  end

  context 'when no attributes are added' do
    it 'should generate correct XML' do
      xml = @item.to_xml
      expect(xml.has_attributes?).to be(false)
      expect(xml.has_elements?).to be(false)
    end
  end

  context 'when all item attributes added, but no subelements' do
    it 'should generate correct XML' do
      xml = @item.tap do |item|
        item.uid = 'uid here'
        item.arg = 'arg here'
        item.autocomplete = 'autocomplete here'
        item.type = 'type here'
        item.valid = true
      end.to_xml

      expect(xml.name).to eql('item')
      expect(xml.attribute('uid').value).to eql('uid here')
      expect(xml.attribute('arg').value).to eql('arg here')
      expect(xml.attribute('autocomplete').value).to eql('autocomplete here')
      expect(xml.attribute('type').value).to eql('type here')
      expect(xml.attribute('valid').value).to eql('yes')
    end
  end

  context 'when adding multiple subtitles' do
    it 'should generate correct XML' do
      @item.add_subtitle('Subtitle 1')
      @item.add_subtitle('Subtitle 2', mod: :fn)

      xml = @item.to_xml
      expect(xml.children.size).to eq(2)
      expect(xml.children[0].text).to eql('Subtitle 1')
      expect(xml.children[1].text).to eql('Subtitle 2')
      expect(xml.children[1].attribute('mod').value).to eql('fn')
    end
  end
end
